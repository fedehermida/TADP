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

    def initialize(klass)

      @clase = klass

    end

    def === unaClase

      if unaClase.is_a?(self.clase)
        return true
      else
        return false

      end

    end


  end

  def is_a clase

    Patron_is_a.new(clase)

  end

  class Patron_has

    attr_accessor :atributo, :valor

    def initialize(unAtributo,unValor)

      @atributo = unAtributo
      @valor = unValor


    end

    def === objeto



      if objeto.array_atributos.include?(atributo.to_s)
        atributoArroba = ("@"+atributo.to_s).to_sym
        if objeto.instance_variable_get(atributoArroba) == self.valor
          puts "El patron matchea"
          true

        else
          puts "El patron falla: el #{atributo} no es #{valor}"
          false

        end
      else
        puts "El patron falla: no hay atributo #{atributo}"
        false
      end

    end


  end


  def has(nombre, atributo)

    patron =Patron_has.new(nombre,atributo)


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
            instance_variable_set("#{valor.to_s}",args.at(indice))
          end
        else
          raise "wrong number of arguments (given #{args.size.to_s}, expected #{self.class.instance_variables.size})"
        end
      end

      def obtener_array_atributos
        instance_variables.map{|ivar| instance_variable_get ivar}
      end

      def array_atributos
        temp = instance_variables.map{|ivar| ivar.to_s.delete("@")}
        temp
      end


      def === (otroCaseClass)
        atributosPropios = otroCaseClass.obtener_array_atributos
        atributosOtros = self.obtener_array_atributos

        listaZip = atributosOtros.zip atributosPropios

        booleanos = listaZip.map { |a| a.first === a.last }
        if (booleanos.any? {|a| a == false})

          return false
        else
          return true
        end

      end



      def to_s
        atributosString = self.obtener_array_atributos.map{|var| var.to_s}
        "#{self.class.name}(#{atributosString.join(", ")})"
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
        raise RuntimeError.new("Herencia no permitida")
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

    if(objeto.instance_variables.size > 0)
      raise "Los Case Objects no pueden tener atributos"
    end

    define_singleton_method objeto.to_s.to_sym do
      objeto
    end

    objeto.singleton_class.class_eval do

      def ===(arg)

        if(self.class == arg.class)

         return self.to_s == arg.to_s
        else
          return false
         end


    end
  end


    objeto.singleton_class.send (define_method :to_s do

      name.to_s

    end)

    objeto.singleton_class.send (define_method :copy do

      objeto.clone

    end)

    objeto.freeze

  end

  class ::Object
    def self.const_missing const
      Selector.new(const)



    end
  end


end

include Inmutabilidad
include Pattern_Matching





