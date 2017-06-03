#Se crea la clase selector la cual deriva de la aparicion de un const_missing, el cual crea una instancia del mismo.
# La instancia entiende el mensaje <
#Para asociar una superclase

class Selector
  attr_accessor :clase, :superclase
  def initialize(unaClase)
    @clase = unaClase
  end
  def <(unaClase)
    @superclase =unaClase
    return self
  end
end

module Pattern_Matching

  #Se define para cada caso un mensaje, ya sea _, has o is_a. Dentro del patron creado cada uno
  # responde al === de una manera distinta.

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

module CaseClass
  #Son los metodos que van a entender las instancias de la clase. Es decir todos los objetos a los que se les haga
  # Class.new
  module InstanceMethodsCaseClass

    #Verifico que la cantidad de argumentos pasados sea igual a la cantidad de atributos de instancia de la clase
    #y seteo cada uno con el valor correspondiente. Si la cantidad de argumentos no coincide, se lanza un error.

    def initialize(*args)
      if(args.size == self.class.instance_variables.size)
        self.class.instance_variables.each_with_index do |valor, indice|
          instance_variable_set("#{valor.to_s}",args.at(indice))
        end
      else
        raise "wrong number of arguments (given #{args.size.to_s}, expected #{self.class.instance_variables.size})"
      end
    end

    #Metodo que obtiene los valores de cada atributo de la clase

    def obtener_array_valores_de_atributos
      instance_variables.map{|ivar| instance_variable_get ivar}
    end

    #Metodo que obtiene el nombre de todos los atributos de la clase

    def array_atributos
      instance_variables.map{|ivar| ivar.to_s.delete("@")}
    end

    #Verifica si todos los atributos de la clase pasada por parametro y los propios coinciden en todos sus valores.

    def === (otroCaseClass)
      atributosPropios = otroCaseClass.obtener_array_valores_de_atributos
      atributosOtros = self.obtener_array_valores_de_atributos
      listaZip = atributosOtros.zip atributosPropios
      listaZip.all? { |a| a.first === a.last }
    end

    # Retorna el nombre de la case class del receptor, seguido del valor de sus atributos entre paréntesis,
    # separados por coma.
    # Ejemplo: "Alumno(Maxi,10)" (Tiene el atributo "nombre" y "nota" seteados como "Maxi" y 10, respectivamente)

    def to_s
      atributosString = self.obtener_array_valores_de_atributos.map{|var| var.to_s}
      "#{self.class.name}(#{atributosString.join(", ")})"
    end

    # Verifica si el objeto pasado por parametro tiene es de la misma clase y tiene seteado
    # los mismos valores de atributos

    def ==(objeto)
      super
      atributosPropios = self.obtener_array_valores_de_atributos
      atributosObjetos = objeto.obtener_array_valores_de_atributos
      self.class == objeto.class && atributosPropios == atributosObjetos
    end

    # Devuelve la operación de sumar 7 más el valor de hash de cada atributo de la clase

    def hash
      lista_hash = self.obtener_array_valores_de_atributos.map {|var| var.hash}
      sum = 7
      sumar_lista_hash = lista_hash.each { |a| sum+=a }
      sum
    end

    # Retorna una nueva instancia con el mismo estado interno del receptor, en el caso de que no se pasen parametros.
    # Si paso por parametro lambdas de aridad 1, cada lambda recibida debe evaluarse sobre el atributo del receptor
    # que lleve el mismo nombre que el parámetro y el resultado debe reemplazar a valor original en la nueva copia.

    # Ejemplo 1:
    #   alumno ​=​ ​Alumno​(​"Jose"​,​ ​8)
    #   otro_alumno ​=​ alumno​.​copy ​->(​nota​){​nota ​+​ ​1}
    #   otro_alumno​.​nombre  ​# Retorna "Jose"
    #   otro_alumno​.​nota    ​# Retorna 9

    # Ejemplo 2:
    #   otro_alumno_mas ​=​ alumno​.​copy ​->(​nombre​){​"Arturo"​},​ ​->(​nota​){​5}
    #   otro_alumno_mas​.​nombre  ​# Retorna "Arturo"
    #   otro_alumno_mas​.​nota    ​# Retorna 5

    # Notar que el nombre del parametro de la lambda necesariamente tiene que corresponderse con el atributo a pisar
    # de otra forma, se tira un error

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

  # Son los metodos que entiende la Clase en si. por ejemplo Class.inherited es un metodo de clase donde self es la
  # propia clase.

  module ClassMethodsCaseClass

    def inherited(subclass)
      raise RuntimeError.new("Herencia no permitida")
    end

    def attr_accessor(*args)
      args.each do |argumento|
        define_method argumento do
          instance_variable_get "@#{argumento.to_s}"
        end
        send(:instance_variable_set, "@#{argumento.to_s}".to_sym, 0)
      end
    end
  end

  #se define un metodo para el mixin CaseClass que incluye los metodos de clase e instancia llamado included.
  def self.included base
    base.send :include, InstanceMethodsCaseClass
    base.extend ClassMethodsCaseClass
  end

end

module Inmutabilidad


  #El metodo case template se lo incluye al mensaje main object y lo que hace es crear una template de la clase

  def caseTemplate(arg)

    if arg
      clazz = Class.new(arg)
    else
      clazz = Class.new
    end

    #Se incluye el mixin CaseClass
    clazz.include(CaseClass)
    clazz

  end



  def case_class instanciaSelector, &block

    #Ejecutado el metodo case_class nos entra como parametro una instancia de Selector.new la cual incluira o no una
    # superclase y un block a evaluar


    #Se asigna el nombre de la clase a una variable temporal
    nombreClase = instanciaSelector.clase

    #Devuelve el template de la case_class
    case_clase = caseTemplate(instanciaSelector.superclase)

    #Se setea y asocia la constante al nombre de la clase
    Object.const_set nombreClase, case_clase

    #Se evalua el bloque en la clase
    case_clase.class_eval &block

    case_clase.freeze


    #Se define la funcion constructora de la case_class
    define_singleton_method case_clase.to_s.to_sym do
    |*args|
      instancia = case_clase.new(*args)
      instancia.freeze
    end

  end


  #Cada vez que no se encuentre una constante se creara una instancia de selector con la constante como parametro
  class ::Object
    def self.const_missing const
      Selector.new(const)
    end
  end




  def case_object instanciaSelector, &block

    nombreCaseObject = instanciaSelector.clase

    case_clase_auxiliar = caseTemplate(instanciaSelector.superclase)

    #Funciones que se comportan distinto en el case_object

    funciones_case_object = proc do

      def ===(arg)
        if(self.class == arg.class)
          return self.to_s == arg.to_s
        end
      end

      #Se devuelve el hash del nombre de la clase

      def hash
        to_s.hash
      end

      def copy
        self.clone
      end

    end

    #Se evalua el bloque creado anteriormente
    case_clase_auxiliar.class_eval &funciones_case_object
    #Se borra el attr_accessor para que no se puedan setear atributos
    case_clase_auxiliar.singleton_class.class_eval {undef :attr_accessor}

    #Se instancia una case_class para sacar el case object
    case_objeto = case_clase_auxiliar.new


    #Se define el to_s
    case_objeto.define_singleton_method :to_s do
      nombreCaseObject.to_s
    end

    #Se setea la constante del objeto

    Object.const_set nombreCaseObject, case_objeto

    #Se evalua el bloque que entro en la definicion principal

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