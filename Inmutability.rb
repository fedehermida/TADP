module Inmutabilidad


  def case_class name, &block

    clazz = Class.new
    Object.const_set name, clazz
    clazz.class_eval &block

    clazz.class_eval do

      def initialize(*args)
        self.class.instance_variables.each_with_index do |valor, indice|
          instance_variable_set("#{valor.to_s}",args.flatten.at(indice))
        end
      end

    end

    define_singleton_method clazz.to_s.to_sym do
    |*args|
      if(args.size == clazz.instance_variables.size)

        instancia = clazz.new(args)
        instancia

      else
        raise "wrong number of arguments (given #{args.size.to_s}, expected #{clazz.instance_variables.size})"
      end
    end
  end

  def case_object name, &block

    objeto = Object.new
    Object.const_set name, objeto
    objeto.instance_eval(&block)

    define_singleton_method objeto.to_s.to_sym do
      objeto
    end
  end

  class ::Object
    def self.const_missing const
      tito = const.to_sym
      tito
    end
  end

end

include Inmutabilidad

case_class Guerrero do
  attr_reader :ataque, :defensa
  @ataque = 0
  @defensa = 0
  #INICIALIZACION DE VARIABLES OBLIGATORIA!!!!!!!
end

case_object Scarcella do

  def volar
    puts "Andar en ojotas con 10 grados"
  end

  def dar_clase
    puts "Hago todo con bloques para que no entiendas un carajo"
  end

end

unGuerrero = Guerrero(40,50)#LOS PARAMETROS SE PASAN EN EL ORDEN QUE FUERON DECLARADOS EN ATTR_READER
unGuerrero.ataque
unGuerrero.defensa

scarcella = Scarcella
scarcella.volar
scarcella.dar_clase