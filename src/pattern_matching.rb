class Patron_
  def ===(algo)
    true
  end
end

def _
  patron = Patron_.new
end

class Patron_is_a
  attr_accessor :clase
  def initialize(clase)
    @clase = clase
  end

  def ===(algo)
    algo.is_a? self.clase
  end
end

def is_a clase
  patron = Patron_is_a.new(clase)
end

class Patron_has
  attr_accessor :atributo, :valor
  def initialize(atributo,valor)
    @atributo = atributo
    @valor = valor
  end

  def ===(algo)
    algo.send(self.atributo) == self.valor
  rescue NoMethodError
    false
  end
end

def has(atributo,valor)
  patron = Patron_has.new(atributo,valor)
end