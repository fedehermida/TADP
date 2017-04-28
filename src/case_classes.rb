
class Symbol
  def < arg
    if arg.is_a? Class
      return [self,arg]
    end
    super < arg
  end
end


module Inmutabilidad



  def case_class name, &block

    if name.is_a? Array
      clazz = Class.new(name[1])
      name = name[0]
    else
      clazz = Class.new
    end
    Object.const_set name, clazz



    clazz.class_eval do

      def initialize(*args)
        if(args.size == self.class.instance_variables.size)
          self.class.instance_variables.each_with_index do |valor, indice|
            instance_variable_set("#{valor.to_s}",args.flatten.at(indice))
          end
        else
          raise "wrong number of arguments (given #{args.size.to_s}, expected #{self.class.instance_variables.size})"
        end
      end

      def method_missing(symbol,*args)

        raise "Error! El metodo #{symbol.to_s} no esta definido!"


      end


      def self.inherited(subclass)
        raise "Herencia no permitida"
      end
    end

    clazz.define_singleton_method :attr_accessor do
    |*args|

      args.each do |argumento|
        define_method argumento do
          instance_variable_get "@#{argumento.to_s}"
        end

        clazz.send(:instance_variable_set, "@#{argumento.to_s}".to_sym, 0)
      end
    end



    clazz.class_eval &block
    clazz.freeze

    define_singleton_method clazz.to_s.to_sym do
    |*args|
      instancia = clazz.new(*args)
      instancia.freeze
    end
  end

  def case_object name, &block

    if name.is_a? Array

      raise "Los Case Objects no pueden heredar"

    end

    objeto = Object.new
    Object.const_set name, objeto
    objeto.instance_eval(&block)

    define_singleton_method objeto.to_s.to_sym do
      objeto
    end
  end

  class ::Object
    def self.const_missing const
      const.to_sym

    end
  end


end

include Inmutabilidad

