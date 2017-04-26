module Inmutabilidad


  def case_class name, &block

    clazz = Class.new
    Object.const_set name, clazz
    clazz.class_eval &block

    define_singleton_method clazz.to_s.to_sym do
    |*args|
      if(args.size == clazz.instance_variables.size)

        instancia = clazz.new

        instancia.instance_variables.each_with_index do |valor, indice|
          instance_variable_set("#{valor.to_s}",args.at(indice))
        end

        instancia

        #"Soy un constructor de #{clazz.to_s}"

      else
        raise "Wrong size of arguments. You have given #{args.size.to_s} instead of #{clazz.instance_variables.size}"
      end
    end
  end

  def case_object name, &block

    objeto = Object.new
    Object.const_set name, objeto
    objeto.instance_eval(&block)
    objeto
  end

  class ::Object
    def self.const_missing const
      tito = const.to_sym
      tito
    end
  end

end
