class Selector

  attr_accessor :clases

  def initialize(unaClase)

    @clases = [unaClase]
    return self

  end


  def <(unaClase)

    @clases = @clases + [unaClase]
    return self

  end

end


module Pattern_Matching

  def _
    _ = Object.new
    _.instance_eval do
      def ===(arg)
        true
      end
    end
    return _
  end

  class Patron_is_a

    attr_accessor :clase

    def initialize klass

      @clase = klass

    end

    def === objeto

      objeto.singleton_class.ancestors.include?(self.clase)

    end


  end

  def is_a clase

    Patron_is_a.new(clase)

  end


end






module Inmutabilidad



  def case_class name, &block

    if name.clases.length > 1
      clazz = Class.new(name.clases[1])
      name = name.clases[0]
    else
      clazz = Class.new
      name = name.clases[0]
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

      def obtener_array_atributos
        instance_variables.map{|ivar| instance_variable_get ivar}
      end


      def to_s
        "#{self.class.name}(#{self.obtener_array_atributos.join(", ")})"
      end

      def ==(objeto)
        super
        atributosPropios = self.obtener_array_atributos
        atributosObjetos = objeto.obtener_array_atributos

        self.class == objeto.class && atributosPropios == atributosObjetos

      end

      def hash
        listaHash = self.obtener_array_atributos.map {|var| var.hash}
        sum = 7
        sumarListaHash = listaHash.each { |a| sum+=a }
        sum
      end

      def method_missing(symbol,*args)

        raise "Error! El metodo #{symbol.to_s} no esta definido!"


      end


      def self.inherited(subclass)
        raise "Herencia no permitida"
      end

      def copy(*args)
        copia=self.dup
        lambdas=args.flatten
        lista_parametros= lambdas.map{|lambda|lambda.parameters.last.last}
        lista_zipeada=lambdas.zip(lista_parametros)
        lista_zipeada.map{|unAttr| x=(unAttr.first)
        copia.instance_variable_set("@#{unAttr.last}",x.call(copia.send(unAttr.last)))}
        copia.freeze
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

    if name.clases.length > 1

      raise "Los Case Objects no pueden heredar"
    else

      name = name.clases[0]

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
      Selector.new(const)



    end
  end


end

include Inmutabilidad
include Pattern_Matching





case_class Guerrero do

  attr_accessor :ataque, :defensa





end
class Menem


end


case_object Mono do





end

sofia = Mono
jorge=Guerrero.new(40,50)



puts jorge.defensa
puts jorge.ataque

julio = jorge.copy ->(defensa){defensa+50}, ->(ataque){ataque+60}
puts julio.ataque
puts julio.defensa