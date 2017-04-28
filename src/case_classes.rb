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
    clazz.class.send(:define_method, :<,Proc.new{ |m| puts "esto hereda de"} )
    clazz.class_eval &block

    clazz.class_eval do


      def initialize(*args)
        self.class.instance_variables.each_with_index do |valor, indice|
          instance_variable_set("#{valor.to_s}",args.flatten.at(indice))
        end
      end

      def self.inherited(subclass)
        raise "No se puede heredar de una Case_Class"
      end


    end




    define_singleton_method clazz.to_s.to_sym do
    |*args|
      if(args.size == clazz.instance_variables.size)

        instancia = clazz.new(args)
        instancia

      else
        raise "Numero de argumentos erroneos (se dieron #{args.size.to_s}, se esperaban #{clazz.instance_variables.size})"
      end
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

class Menem
  def hola
    puts "menem"
  end

end

case_class Guerrero  do
  attr_reader :ataque, :defensa
  @ataque = 0
  @defensa = 0
  #INICIALIZACION DE VARIABLES OBLIGATORIA!!!!!!!
end





case_object Scarcella < Menem do

  def volar
    puts "Andar en ojotas con 10 grados"
  end

  def dar_clase
    puts "Me encantan los bloques"
  end

end

unGuerrero = Guerrero(40,50)#LOS PARAMETROS SE PASAN EN EL ORDEN QUE FUERON DECLARADOS EN ATTR_READER
puts (unGuerrero.ataque)
puts (unGuerrero.defensa)

scarcella = Scarcella
scarcella.volar
scarcella.dar_clase
puts Guerrero.class