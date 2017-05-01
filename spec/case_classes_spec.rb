require_relative '../src/case_classes'
require 'rspec'




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

  it 'test creacion de case_object'do
    case_object Objeto_inmutable do

    end

    expect(Objeto_inmutable.class).to eq(Object)

  end

  it 'test case_object is frozen'do
    case_object Objeto_inmutable2 do

    end

    expect(Objeto_inmutable2.frozen?).to eq(true)
  end

  it 'test case_class is frozen instances' do
    case_class Guerrero3 do
      attr_accessor :ataque, :defensa
    end

    jorge=Guerrero3(40,50)

    expect(jorge.frozen?).to eq(true)
  end


  it 'test case_class con herencia ilegal' do
    case_class  Guerrero4 do
    end

    lambda=proc{case_class Milicia < Guerrero4 }


    expect(lambda.call).to raise_error(RuntimeError)
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
    julio=jorge.copy ->(fuerza){fuerza+30}


    expect(julio.fuerza).to raise_error(NoMethodError)
  end

  it 'test modificar la instacia de una case class,error no se puede estan frizadas' do
    case_class Guerrero7 do
      attr_accessor :ataque, :defensa
    end
    jorge=Guerrero7(40,50)

    expect(jorge.defensa=30).to raise_error(NoMethodError)

  end


end

