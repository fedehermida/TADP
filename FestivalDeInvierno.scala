package main


package object FestivalDeInvierno {
  
  trait Competidor {
    def cargarPescados(cantidad:Int){
      require(0 to this.cantMaxPescados() contains cantidad, "Cantidad no valida")
    }
    def barbarosidad:Int
    def cantMaxPescados():Int
    def danio:Int
    def tieneItem(unItem:Item):Boolean
    def velocidad:Int
    def hambre:Int
    def aumentarHambre(cantidad:Int):Competidor
    def esMejorQue(unCompetidor: Competidor)(unaPosta: Posta): Boolean =
      unaPosta match{
      case Pesca(_,_) => this.cantMaxPescados() > unCompetidor.cantMaxPescados()
      case Combate(_,_) => this.danio > unCompetidor.danio
      case Carrera(_,_) => this.velocidad > unCompetidor.velocidad
    }
    
  }

  case class Jinete(unVikingo:Vikingo,unDragon:Dragon) extends Competidor{
    def cantMaxPescados():Int = unDragon.peso / 5 - unVikingo.peso
    def danio=unVikingo.danio + unDragon.danio
    def hambre=unVikingo.caracteristicas.hambre
    def barbarosidad=unVikingo.barbarosidad
    def tieneItem(unItem:Item) = unVikingo.tieneItem(unItem)
    def aumentarHambre(cantidad:Int) =
      this.copy(unVikingo = unVikingo.aumentarHambre(5))
    
    def velocidad=unDragon.velocidad()-unVikingo.peso
  }


  case class Vikingo(item:Item,
                     caracteristicas: Caracteristicas) extends Competidor{

    def peso = this.caracteristicas.peso
    def velocidad=this.caracteristicas.velocidad
    def hambre=this.caracteristicas.hambre
    def barbarosidad=this.caracteristicas.barbarosidad
    def tieneItem(unItem:Item):Boolean = item == unItem
    def danio=this.barbarosidad + item.danio
    
    def aumentarHambre(cantidad:Int) =
      this.copy(caracteristicas = caracteristicas.copy(hambre = caracteristicas.hambre + cantidad))

    
    def cantMaxPescados():Int = peso / 2 + barbarosidad * 2
    
    def montar(unDragon:Dragon): Option[Jinete] =
      if (this.puedeMontar(unDragon))
        Some(Jinete(this,unDragon))
      else
        None
      
    def puedeMontar(unDragon:Dragon): Boolean =
      unDragon.requisitos.foldRight(true)((a,b) => a(this, unDragon).&&(b))
      
    def mejorMontura(dragones:List[Dragon])(unaPosta:Posta):Competidor = {
      val jinetes = dragones
          .flatMap(dragon => this.montar(dragon)
          .flatMap{jinete => if (unaPosta.puedeParticipar(jinete)) Some(jinete) else None})
        jinetes.foldRight(this:Competidor){(jinete,competidor) =>
          if (jinete.esMejorQue(competidor)(unaPosta))
            jinete
          else competidor
      }
      
     }
  
  }

  case class Caracteristicas(peso:Int,
                             velocidad:Int,
                             barbarosidad:Int,
                             hambre:Int){

  }

  case class Item(danio:Int){

  }
  
  trait Dragon{
    def requisitos: List[Requisito] = List(RequisitoBasico)
    def peso: Int
    def danio: Int
    def velocidadBase: Int = 60
    def velocidad(): Int = velocidadBase - peso

  }
  
  case class FuriaNocturna(
      peso: Int,
      danio: Int) extends Dragon{
    
    override def velocidad(): Int = {
      super.velocidad()*3
    }
    
    
  }


  case class NadderMortifero(
      peso: Int,
      danio: Int = 150,
      override val requisitos:List[Requisito] = List(RequisitoBasico,RequisitoDeDanio)) extends Dragon{

  }

  case class Gronckle(peso: Int, pesoMaximo:Int) extends Dragon{
    
    override val requisitos:List[Requisito] = List(RequisitoDePeso(pesoMaximo))
    override def velocidadBase = super.velocidadBase / 2
    def danio = peso * 5
   

  }
  
  sealed trait Requisito {
    def apply(unVikingo:Vikingo, unDragon:Dragon):Boolean
  }
  
  case object RequisitoBasico extends Requisito{
    def apply(unVikingo:Vikingo, unDragon:Dragon):Boolean =
      unVikingo.peso <= unDragon.peso / 5
  }
  
  case class RequisitoBarbarosidad(minimo:Int) extends Requisito{
    def apply(unVikingo:Vikingo, unDragon:Dragon):Boolean =
      unVikingo.barbarosidad > this.minimo
  }
  
  case class RequisitoDeItem(unItem:Item) extends Requisito{
    def apply(unVikingo:Vikingo, unDragon:Dragon):Boolean =
      unVikingo.tieneItem(unItem)
  }
  
  case object RequisitoDeDanio extends Requisito{
    def apply(unVikingo:Vikingo, unDragon:Dragon):Boolean =
      unVikingo.danio < unDragon.danio
  }
  
  case class RequisitoDePeso(pesoMaximo: Int) extends Requisito{
    def apply(unVikingo:Vikingo, unDragon:Dragon):Boolean =
      unVikingo.peso <= pesoMaximo
  }
  
  sealed trait CriterioAdmision
  
  case class CriterioHambre(hambre:Int) extends CriterioAdmision{
    def apply(unCompetidor: Competidor):Boolean =
      unCompetidor.aumentarHambre(hambre).hambre <= 100
  }
  
  case class CriterioPesoCarga(minimo:Int) extends CriterioAdmision{
    def apply(unCompetidor: Competidor):Boolean =
      unCompetidor.cantMaxPescados() > minimo
  }
  
  case class CriterioBarbarosidadMinima(minimo:Int) extends CriterioAdmision{
    def apply(unCompetidor: Competidor):Boolean =
      unCompetidor.barbarosidad > minimo
  }
  
  case class CriterioDeArma(unArma: Item) extends CriterioAdmision{
    def apply(unCompetidor: Competidor):Boolean =
      unCompetidor.tieneItem(unArma)
  }
  
  case class CriterioMontura() extends CriterioAdmision{
    def apply(unCompetidor: Competidor):Boolean =
      unCompetidor.isInstanceOf[Jinete]
  }
  
  trait Posta{
    def hambreProvocada:Int
    def cumpleCriterio(unCompetidor: Competidor):Boolean
    def puedeParticipar(unCompetidor: Competidor):Boolean =
      CriterioHambre(this.hambreProvocada)(unCompetidor) && this.cumpleCriterio(unCompetidor)
  }
  
  case class Pesca(requisito: Option[CriterioPesoCarga],hambreProvocada:Int = 5) extends Posta{
    override def cumpleCriterio(unCompetidor: Competidor) =
      requisito match {
      case Some(_) => requisito.get(unCompetidor)
      case None => true
    }
  }
  
  case class Combate(requisito: Either[CriterioBarbarosidadMinima,CriterioDeArma],hambreProvocada:Int = 10) extends Posta{
    override def cumpleCriterio(unCompetidor: Competidor) =
      requisito match {
      case Left(CriterioBarbarosidadMinima(_)) => requisito.left.get(unCompetidor)
      case Right(CriterioDeArma(_)) => requisito.right.get(unCompetidor)
    }
  }
  
  case class Carrera(requisito: Option[CriterioMontura],distancia:Int) extends Posta{
    def hambreProvocada = distancia
    override def cumpleCriterio(unCompetidor: Competidor) =
      requisito match{
      case Some(unRequisito) => unRequisito(unCompetidor)
      case None => true
    }
  }
  trait EstadoTorneo{
    def participantes:List[Competidor]
    def mapearAVikingo(competidores: List[Competidor]):List[Vikingo] =
      competidores.map (unCompetidor => this.aVikingo(unCompetidor))
  
    def aVikingo(competidor: Competidor): Vikingo = competidor match {
        case Jinete(unVikingo,_) => unVikingo
        case Vikingo(item,caracteristicas) => Vikingo(item,caracteristicas)
        }
  }
  
  case class Ronda(participantes: List[Competidor]) extends EstadoTorneo{
    def realizar(unaPosta: Posta): EstadoTorneo = {
      val participantesFiltrados = participantes.filter(unaPosta.puedeParticipar(_))
      participantesFiltrados match {
        case participante :: resto if resto.size > 1 =>
          val resultantes = (participante::resto).sortWith((p1,p2) => p1.esMejorQue(p2)(unaPosta)).map(_.aumentarHambre(unaPosta.hambreProvocada))
          Ronda(resultantes)
        case participante :: Nil => Finalizado(List(participante))
        case Nil => Finalizado(List())
      }
    }
  }

  
  case class Finalizado(participantes: List[Competidor]) extends EstadoTorneo{
    def realizar(unaPosta: Posta): EstadoTorneo = this
  }
 
  case class Torneo(postas: List[Posta], dragonesDisponibles: List[Dragon], reglas: Reglas){
    def realizar(participantes: List[Vikingo]) = 
      postas.foldLeft(Ronda(participantes):EstadoTorneo) { (rondaAnterior, postaActual) => 
        val participantesPreparados = reglas.elegirMontura(rondaAnterior.mapearAVikingo(rondaAnterior.participantes))(dragonesDisponibles)
        rondaAnterior match {
          case Ronda(_) =>
            val resultado = Ronda(participantesPreparados).realizar(postaActual)
            Ronda(reglas.avanzanASiguiente(resultado.participantes))
          
          case Finalizado(participantes) => Finalizado(participantes)
        
       }
     }
  }
  
  trait Reglas{
    def elegirMontura(participantes:List[Vikingo])(dragonesDisponibles:List[Dragon]):List[Competidor]
    def avanzanASiguiente(participantes:List[Competidor]):List[Competidor]
    def ganador(participantes:List[Competidor]):Option[Vikingo]
  }
  
  case class ReglasEstandar(){
  }
}