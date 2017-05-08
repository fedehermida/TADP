require_relative '../src/case_classes'
require 'rspec'

case_object X do

  def prueba
    "hello world"
  end

  def self.prueba1
    "Satisfactorio"
  end


end


describe 'test de case_classes' do
  it 'test declarar una case_class' do

    case_class Monje do
    end

    expect(Monje.class).to eq(Class)

  end

  it 'test constructor con cambio sintactico' do
    case_class Guerrero do
      attr_accessor :ataque, :defensa
    end

    jorge= Guerrero(40,50)
    expect(jorge.class).to eq(Guerrero)
  end

  it 'test buenos default: to_s' do
    case_class Guerrillero do
      attr_accessor :ataque, :defensa
    end

    jorge= Guerrillero(40,50)

    expect(jorge.to_s).to eq("Guerrillero(40, 50)")
  end
  it 'test buenos default: ==' do
    case_class Guerrero1 do
      attr_accessor :ataque, :defensa
    end
    jorge=Guerrero1(40,50)
    julio=Guerrero1(40,50)

    expect(jorge==julio).to eq(true)
  end

  it 'test copy sin lambdas'do
    case_class Guerrero2 do
      attr_accessor :ataque, :defensa
    end
    jorge=Guerrero2(40,50)
    julio=jorge.copy

    expect(jorge==julio).to eq(true)
  end


  it 'test copy con lambdas'do
    case_class Guerrero8 do
      attr_accessor :ataque, :defensa
    end
    jorge=Guerrero8(40,50)
    julio=jorge.copy ->(defensa){defensa+30},->(ataque){ataque+20}

    expect(julio.defensa).to eq(80)
    expect(julio.ataque).to eq(60)
  end

  it 'test case_class define metodos de clase correctamente' do
    case_class Fighter1 do
      attr_accessor :ataque, :defensa
      def hola1
        "hola"
      end

    end
    jorge=Fighter1(40,50)

    expect(jorge.hola1).to eq("hola")

  end

  it 'test case_class define singleton methods correctamente' do
    case_class Fighter2 do
      attr_accessor :ataque, :defensa
      def hola1
        "hola"
      end

      def self.hola2
        "como estas"
      end

    end

    expect(Fighter2.hola2).to eq("como estas")

  end

  it 'test instancia de case_class no encuentra metodo de la singleton class' do
    case_class Fighter3 do
      attr_accessor :ataque, :defensa
      def self.hola1
        "hola"
      end

    end
    jorge=Fighter3(40,50)

    expect{ jorge.hola1}.to raise_error(NoMethodError)

  end

  it 'test creacion de case_object'do


    expect(X.is_a?(Object)).to eq(true)

  end

  it 'test case_object is frozen'do

    expect(X.frozen?).to eq(true)
  end

  it 'test case_object define metodos de instancia correctamente'do

    expect(X.prueba).to eq("hello world")
  end

  it 'test case_object define auto-metodos correctamente'do

    expect(X.prueba1).to eq("Satisfactorio")
  end

  it 'test case_object intenta setear atributos'do

    expect{ case_object Y do attr_accessor :propiedad end }.to raise_error(NoMethodError)
  end

  it 'test case_object no tienen hash'do

    expect{ X.hash }.to raise_error(NoMethodError)
  end


  it 'test case_class is frozen instances' do
    case_class Guerrero3 do
      attr_accessor :ataque, :defensa
    end

    jorge=Guerrero3(40,50)

    expect(jorge.frozen?).to eq(true)
  end


  it 'test case_class con herencia ilegal' do
    case_class Milicia do


    end

    expect{
      class Guerrero4 < Milicia

      end

      }.to raise_exception("Herencia no permitida")
  end

  it 'test case_class puede ser extension de una clase' do
    case_class Guerrero5 < Fixnum do

    end

    expect(Guerrero5.class).to eq(Class)

  end

  it 'test copy con lambdas, error en los nombres de los parametros distintos a los de los atributos'do
    case_class Guerrero6 do
      attr_accessor :ataque, :defensa
    end
    jorge=Guerrero6(40,50)



    expect{
      julio=jorge.copy ->(fuerza){fuerza+30}

      julio.fuerza}.to raise_error(RuntimeError)
  end

  it 'test modificar la instacia de una case class, desde un setter' do
    case_class Guerrero7 do
      attr_accessor :ataque, :defensa
    end
    jorge=Guerrero7(40,50)

    expect{jorge.defensa=30}.to raise_error(NoMethodError)

  end

  it 'test modificar la instacia de una case class, desde un metodo' do
    case_class Fighter8 do
      attr_accessor :ataque, :defensa

      def hacerTrampa
        @ataque = 1000000
      end


    end
    jorge=Fighter8(40,50)

    expect{jorge.hacerTrampa}.to raise_error(RuntimeError)

  end


  it 'test pattern matching _ siempre es true' do
    objeto = Object.new
    valor = case objeto
              when _
                true
            end



    expect(valor).to eq(true)

  end

  it 'test pattern matching is_a es de su propia clase' do
    case_class Guerrero10 do
      attr_accessor :ataque, :defensa
    end


    jorge=Guerrero10(40,50)

    valor = case jorge
              when is_a(Guerrero10)
                true
            end



    expect(valor).to eq(true)
  end

  it 'test pattern matching is_a pertenece a sus ancestros' do
    case_class Guerrero11 do
      attr_accessor :ataque, :defensa
    end


    jorge=Guerrero11(40,50)

    valor = case jorge
              when is_a(Object)
                true
            end



    expect(valor).to eq(true)
  end
  it 'test pattern matching is_a no pertenece a la clase Array' do
    case_class Guerrero12 do
      attr_accessor :ataque, :defensa
    end


    jorge=Guerrero12(40,50)

    valor = case jorge
              when is_a(Array)
                true
            end



    expect(valor).to eq(nil)
  end
  it 'test pattern matching has posee el atributo y coincide el valor' do
    case_class Guerrero13 do
      attr_accessor :ataque, :defensa
    end


    jorge=Guerrero13(40,50)

    valor = case jorge
              when has(:ataque, 40)
                true
            end



    expect(valor).to eq(true)
  end
  it 'test pattern matching has posee el atributo pero no coincide el valor' do
    case_class Guerrero14 do
      attr_accessor :ataque, :defensa
    end


    jorge=Guerrero14(40,50)

    valor = case jorge
              when has(:ataque, 90)
                true
            end



    expect(valor).to eq(nil)
  end
  it 'test pattern matching has no posee el atributo' do
    case_class Guerrero15 do
      attr_accessor :ataque, :defensa
    end


    jorge=Guerrero15(40,50)

    valor = case jorge
              when has(:escudo, 90)
                true
            end



    expect(valor).to eq(nil)
  end
  it 'test pattern matching case_cases no coincide la nota' do
    case_class Alumno do

      attr_accessor :nombre, :termino


    end

    case_class Termino do

      attr_accessor :nota

    end


    alumno = Alumno("Jose", Termino(9))
    valor = case alumno
              when Alumno( "Jose", Termino(7) )
                true
            end



    expect(valor).to eq(nil)
  end
  it 'test pattern matching case_cases no coincide el nombre ' do
    case_class Alumno1 do

      attr_accessor :nombre, :termino


    end

    case_class Termino1 do

      attr_accessor :nota

    end


    alumno = Alumno1("Roberto", Termino1(9))
    valor = case alumno
              when Alumno1( "Jose", Termino1(9) )
                true
            end



    expect(valor).to eq(nil)
  end
  it 'test pattern matching case_cases matchea ' do
    case_class Alumno2 do

      attr_accessor :nombre, :termino


    end

    case_class Termino2 do

      attr_accessor :nota

    end


    alumno = Alumno2("Roberto", Termino2(9))
    valor = case alumno
              when Alumno2( "Roberto", Termino2(9) )
                true
            end



    expect(valor).to eq(true)
  end
  it 'test pattern matching case_cases falla atributos distintos ' do
    case_class Alumno3 do

      attr_accessor :nombre, :termino


    end

    case_class Termino3 do

      attr_accessor :nota

    end




    alumno = Alumno3("Roberto", Termino3(9))
    valor = case alumno
              when Alumno3( "Roberto", X )
                true
            end



    expect(valor).to eq(nil)
  end


end
