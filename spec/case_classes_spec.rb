require_relative '../src/case_classes'
require 'rspec'


describe 'test de case_classes' do
  it 'test declarar una case_class' do

    case_class Monje do
    end

    expect(Monje.class).to eq(Monje)

  end

  it 'test constructor con cambio sintactico' do
    case_class Guerrero do
      attr_accessors :ataque, :defensa
    end

    jorge= Guerrero(40,50)
    expect(jorge.class).to eq(Object)
  end

  it 'test buenos default: to_s' do
    case_class Guerrero do
      attr_accessors :ataque, :defensa
    end

    jorge= Guerrero(40,50)

    expected(jorge.to_s).to eq("Guerrero(40,50)")
  end
  it 'test buenos default: ==' do
    case_class Guerrero do
      attr_accessors :ataque, :defensa
    end
    jorge=Guerrero(40,50)
    julio=Guerrero(40,50)

    expected(jorge==julio).to eq(true)
  end

  it 'test copy sin lambdas'do
    case_class Guerrero do
      attr_accessors :ataque, :defensa
    end
    jorge=Guerrero(40,50)
    julio=jorge.copy

    expected(jorge==julio).to eq(true)
  end


  it 'test copy con lambdas'do
    case_class Guerrero do
      attr_accessors :ataque, :defensa
    end
    jorge=Guerrero(40,50)
    julio=jorge.copy ->(defensa){defensa+30},->(ataque){ataque+20}

    expected(julio.defensa).to eq(80)
    expected(julio.ataque).to eq(60)
  end

  it 'test creacion de case_object'do
    case_object Objeto_inmutable do

    end

    expected(Objeto_inmutable.class).to eq(Object)

  end

  it 'test case_object is frozen'do
    case_object Objeto_inmutable do

    end

    expected(Objeto_inmutable.frozen?).to eq(true)
  end

  it 'test case_class is frozen instances' do
    case_class Guerrero do
    attr_accessors :ataque, :defensa
    end

    jorge=Guerrero(40,50)

    expect(jorge.frozen?).to eq(true)
  end


  it 'test case_class con herencia ilegal' do
  case_class  Guerrero do
       end
  expect(case_class Milicia < Guerrero ).to raise_error(RuntimeError)
  end

  it 'test case_class puede ser extension de una clase' do
  case_class Guerrero < Fixnum do

  end

    expect(Guerrero.class).to eq(Class)

  end

  

  end