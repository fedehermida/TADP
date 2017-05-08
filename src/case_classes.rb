class Selector
  attr_accessor :clase, :superclase
  def initialize(unaClase)
    @clase = unaClase
    return self
  end
  def <(unaClase)
    @superclase =unaClase
    return self
  end
end

module Pattern_Matching
  class Patron_
    def === arg
      true
    end
  end

  def _
    Patron_.new
  end

  class Patron_is_a
    attr_accessor :clase
    def initialize(klass)
      @clase = klass
    end
    def === unaClase
      return unaClase.is_a?(self.clase)
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
        return objeto.instance_variable_get(atributoArroba) == self.valor
      end
    end
  end
  def has(nombre, atributo)
    patron =Patron_has.new(nombre,atributo)
  end
end

module Inmutabilidad

 module Methods

   def self.included base
     base.send :include, InstanceMethods
     base.extend ClassMethods
   end


   module ClassMethods

    def inherited(subclass)
      raise RuntimeError.new("Herencia no permitida")
    end



  end

  module InstanceMethods

    def initialize(*args)
      if(args.size == self.class.instance_variables.size)
        self.class.instance_variables.each_with_index do |valor, indice|
          instance_variable_set("#{valor.to_s}",args.at(indice))
        end
      else
        raise "wrong number of arguments (given #{args.size.to_s}, expected #{self.class.instance_variables.size})"
      end
    end
    def obtener_array_valores_de_atributos
      instance_variables.map{|ivar| instance_variable_get ivar}
    end
    def array_atributos
      instance_variables.map{|ivar| ivar.to_s.delete("@")}
    end
    def === (otroCaseClass)
      atributosPropios = otroCaseClass.obtener_array_valores_de_atributos
      atributosOtros = self.obtener_array_valores_de_atributos
      listaZip = atributosOtros.zip atributosPropios
      listaZip.all? { |a| a.first === a.last }
    end
    def to_s
      atributosString = self.obtener_array_valores_de_atributos.map{|var| var.to_s}
      "#{self.class.name}(#{atributosString.join(", ")})"
    end
    def ==(objeto)
      super
      atributosPropios = self.obtener_array_valores_de_atributos
      atributosObjetos = objeto.obtener_array_valores_de_atributos
      self.class == objeto.class && atributosPropios == atributosObjetos
    end
    def hash
      lista_hash = self.obtener_array_valores_de_atributos.map {|var| var.hash}
      sum = 7
      sumar_lista_hash = lista_hash.each { |a| sum+=a }
      sum
    end

    def copy(*args)
      lista_de_atributos=self.instance_variables.map{|attr| attr.to_s.delete("@")}
      lista_parametros= args.flatten.map{|lambda|lambda.parameters.last.last}
      lista_parametros_string=lista_parametros.map{|parametro| parametro.to_s}
      if lista_parametros_string.map{|param| lista_de_atributos.include?(param)}.any?{|cond| cond==false}
        atributoFaltante=((lista_de_atributos+(lista_parametros_string)).uniq-lista_de_atributos).first
        raise "Error no existe el atributo #{atributoFaltante}"
      end
      copia=self.dup
      lista_zipeada=args.flatten.zip(lista_parametros)
      lista_zipeada.map{|unAttr| x=(unAttr.first)
      copia.instance_variable_set("@#{unAttr.last}",x.call(copia.send(unAttr.last)))}
      copia.freeze
    end



  end
 end


  def caseTemplate(arg)

    #si lo que llega es nil lo toma como false, si llega algo distinto es superclase
    if arg
      clazz = Class.new(arg)
    else
      clazz = Class.new
    end

    clazz.include(Methods)

    clazz.define_singleton_method :attr_accessor do
    |*args|
      args.each do |argumento|
        define_method argumento do
          instance_variable_get "@#{argumento.to_s}"
        end
        clazz.send(:instance_variable_set, "@#{argumento.to_s}".to_sym, 0)
      end
    end

    clazz


  end



  def case_class instanciaSelector, &block

    nombreClase = instanciaSelector.clase

    case_clase = caseTemplate (instanciaSelector.superclase)

    Object.const_set nombreClase, case_clase


    case_clase.class_eval &block

    case_clase.freeze

    define_singleton_method case_clase.to_s.to_sym do
    |*args|
      instancia = case_clase.new(*args)
      instancia.freeze
    end


  end


  class ::Object
    def self.const_missing const
      Selector.new(const)
    end
  end




  def case_object instanciaSelector, &block
    if !(instanciaSelector.superclase.nil?)
      raise "Los Case Objects no pueden heredar"
    else
      nombreCaseObject = instanciaSelector.clase
    end

    case_clase_auxiliar = caseTemplate(nil)

    nombreCaseClassAuxiliar = "#{nombreCaseObject.to_s}_casetemplate"

    Object.const_set nombreCaseClassAuxiliar, case_clase_auxiliar

    funciones_case_object = proc do


      def ===(arg)
        if(self.class == arg.class)
          return self.to_s == arg.to_s
        end
      end

      def copy
        self.clone
      end

      def to_s

        self.class.to_s.gsub("_casetemplate", "")

      end


    end

    case_clase_auxiliar.class_eval &funciones_case_object
    case_clase_auxiliar.class_eval { undef :hash }
    case_clase_auxiliar.singleton_class.class_eval {undef :attr_accessor}

    case_objeto = case_clase_auxiliar.new

    Object.const_set nombreCaseObject, case_objeto

    case_objeto.instance_eval &block

    if(case_objeto.instance_variables.size > 0)
      raise "Los Case Objects no pueden tener atributos"
    end

    case_objeto.freeze

    case_objeto

  end

end

include Inmutabilidad
include Pattern_Matching
