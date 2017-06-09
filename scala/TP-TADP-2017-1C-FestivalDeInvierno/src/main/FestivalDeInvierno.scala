package main


package object FestivalDeInvierno {
  
  trait Competidor {
    def cargarPescados(cantidad:Int){
      require(0 to this.cantMaxPescados() contains cantidad, "Cantidad no valida")
    }
    
    def cantMaxPescados():Int = ???
    def danio:Int = ???
    
  }

  case class Jinete(unVikingo:Vikingo,unDragon:Dragon) extends Competidor{
    override def cantMaxPescados():Int = unDragon.peso / 5 - unVikingo.peso
    
    override def danio=unVikingo.danio + unDragon.danio
    
    def velocidad=unDragon.velocidad()-unVikingo.peso
  }


  case class Vikingo(item:Item,
                     caracteristicas: Caracteristicas) extends Competidor{

    def peso = this.caracteristicas.peso
    def velocidad=this.caracteristicas.velocidad
    def hambre=this.caracteristicas.hambre
    def barbarosidad=this.caracteristicas.barbarosidad
    def tieneItem(unItem:Item):Boolean = item == unItem
    override def danio=this.barbarosidad + item.danio

    
    override def cantMaxPescados():Int = peso / 2 + barbarosidad * 2
    
    def montar(unDragon:Dragon): Competidor =
      if (this.puedeMontar(unDragon))
        Jinete(this,unDragon)
      else
        this
      
    def puedeMontar(unDragon:Dragon): Boolean =
      unDragon.requisitos.foldRight(true)((a,b) => a.cumpleRequisito(this, unDragon).&&(b))
  }

  case class Caracteristicas(peso:Int,
                             velocidad:Int,
                             barbarosidad:Int,
                             hambre:Int){

  }

  case class Item(danio:Int){

  }

  trait Atributos{
    def velocidadBase: Int
    def peso: Int
    def danio: Int
  }
  
  abstract class Dragon(
      val requisitos: List[Requisito] = List(RequisitoBasico)) extends Atributos{
    
    def velocidadBase: Int = 60
    def velocidad(): Int = velocidadBase - peso

  }
  
  case class FuriaNocturna(
      peso: Int,
      danio: Int,
      override val requisitos:List[Requisito]) extends Dragon(requisitos:List[Requisito]){
    
    override def velocidad(): Int = {
      super.velocidad()*3
    }
    
    
  }


  case class NadderMortifero(
      peso: Int,
      danio: Int = 150,
      override val requisitos:List[Requisito] = List(RequisitoBasico,RequisitoDeDanio)) extends Dragon{

  }

  case class Gronckle(
      peso: Int,
      danio: Int,
      override val requisitos:List[Requisito]) extends Dragon(requisitos:List[Requisito]){
    
    override def velocidadBase = super.velocidadBase / 2
   

  }
  
  trait Requisito {
    def cumpleRequisito(unVikingo:Vikingo, unDragon:Dragon): Boolean
  }
  
  case object RequisitoBasico extends Requisito{
    override def cumpleRequisito(unVikingo:Vikingo, unDragon:Dragon):Boolean =
      unVikingo.peso <= unDragon.peso / 5
  }
  
  case class RequisitoBarbarosidad(minimo:Int) extends Requisito{
    def cumpleRequisito(unVikingo:Vikingo, unDragon:Dragon):Boolean =
      unVikingo.barbarosidad > this.minimo
  }
  
  case class RequisitoDeItem(unItem:Item) extends Requisito{
    def cumpleRequisito(unVikingo:Vikingo, unDragon:Dragon):Boolean =
      unVikingo.tieneItem(unItem)
  }
  
  case object RequisitoDeDanio extends Requisito{
    def cumpleRequisito(unVikingo:Vikingo, unDragon:Dragon):Boolean =
      unVikingo.danio < unDragon.danio
  }
  
  case class RequisitoDePeso(pesoMaximo: Int) extends Requisito{
    def cumpleRequisito(unVikingo:Vikingo, unDragon:Dragon):Boolean =
      unVikingo.peso <= pesoMaximo
  }

}