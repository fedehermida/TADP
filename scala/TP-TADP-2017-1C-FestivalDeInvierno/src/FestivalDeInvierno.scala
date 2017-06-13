/**
  * Created by Cabeza de Tacho on 9/6/2017.
  */
object FestivalDelInvierno {

  abstract class Participante {

    def cantidadDeCarga: Double

    def barbarosidad: Int

    def tieneArma: Boolean

    def nivelHambre:Double

    def evaluoParticipacion(posta: Posta):Boolean ={
      if(this.nivelHambre+posta.cuantoHambreAumento()>=100) {
        posta.puedeParticipar(this)
      }
    }

    def danio:Int

    def velocidad:Int

    def soymejorQue(participante:Participante,posta:Posta):Boolean={
      posta.quienEsMejor(this,participante)
    }

    def aumentarHambrePorPosta(posta: Posta): Participante




  }

  case class Vikingo(caracteristica: CaracteristicasVikingo, item: Option[Item]) extends Participante {

    def peso = caracteristica.peso

    override def barbarosidad = caracteristica.barbarosidad

    override def velocidad = caracteristica.velocidad

    override def nivelHambre = caracteristica.nivelHambre

    override def danio = {
      item match {
        case Arma(unDanio) => barbarosidad + unDanio
        case None => barbarosidad
      }
    }

    override def tieneArma: Boolean = item match {

      case Some(Arma(_)) => true
      case _ => false

    }

    def modificarNivelHambre(nuevoHambre: Double) :Vikingo = copy(caracteristica = caracteristica.copy(nivelHambre = nuevoHambre))

    def montar(unDragon: Dragon): Option[Jinete] = {
      if (puedoMontarlo(unDragon))
        Some(Jinete(this, unDragon))
      else
        None
    }

    override def cantidadDeCarga: Double = peso / 2 + barbarosidad * 2

    override def aumentarHambrePorPosta(posta: Posta): Participante = {
      match
      {
        case Pesca(_) => this.copy(nivelHambre = modificarNivelHambre(nivelHambre + 5))
        case Combate(_) => this.copy(nivelHambre = modificarNivelHambre(nivelHambre + 10))
        case Carrera(cantidadDeKilometros, _) => this.copy(nivelHambre = modificarNivelHambre(nivelHambre + cantidadDeKilometros))
      }
    }

    def puedoMontarlo(unDragon: Dragon): Boolean = {
      unDragon.condicionesDeMonturaDragon.forall(condicion => condicion.cumpleRequisito(this, unDragon))
    }



    def meConvieneMontadoONo(dragones: List[Dragon], posta: Posta): Participante = {
      var dragonesQuePuedoMontar = dragones.filter(dragon => puedoMontarlo(dragon))
      var jinetesAElegir = dragonesQuePuedoMontar.map(dragon => dragon.montar)
      return this.soymejorQue(jinetesAElegir.sortWith(posta.quienEsMejor(_, _)).head)
    }
  }

  case class CaracteristicasVikingo(peso: Int, velocidad: Int, barbarosidad: Int, nivelHambre: Double)

  abstract class Item

  case class Arma(unDanio: Int) extends Item

  case object SistemaDeVuelo extends Item

  case class Comestible(disminuirHambre: Double) extends Item


  trait Dragon(velocidadBase: Double,
  peso: Double,
  velocidadDeVuelo: Double,
  danio: Double,
  cargaMaxima: Double,
  cargaActual: Double,
  condicionesDeMonturaDragon: List[CondicionMontura]) {

  }



  case class FuriaNocturna() extends Dragon{
      def velocidadBase= velocidadBase*3
  }

  case class NadderMortifero() extends Dragon{
    def danio = return 150
  }

  case class Gronckle() extends Dragon{
    def danio= peso*5
  }





  abstract class CondicionMontura {
    def cumpleRequisito(unVikingo: Vikingo, unDragon: Dragon): Boolean
  }

  case object RequisitoBasico extends CondicionMontura {
    override def cumpleRequisito(unVikingo: Vikingo, unDragon: Dragon): Boolean = {
      unVikingo.peso <= unDragon.peso / 5
    }
  }

  case class RequisitoBarbarosidad(minimoBarbarosidad: Int) extends CondicionMontura {
    override def cumpleRequisito(unVikingo: Vikingo, unDragon: Dragon): Boolean = {
      unVikingo.barbarosidad > minimoBarbarosidad
    }
  }

  case class RequisitoItem(unItem: Item) extends CondicionMontura {
    override def cumpleRequisito(unVikingo: Vikingo, unDragon: Dragon): Boolean = {
      unVikingo.item == unItem
    }
  }

  case object RequisitoDanio extends CondicionMontura {
    override def cumpleRequisito(unVikingo: Vikingo, unDragon: Dragon): Boolean = {
      unVikingo.danio < unDragon.danio
    }
  }

  case class RequisitoPesoGronckle(pesoMaximo: Int) extends CondicionMontura {
    override def cumpleRequisito(unVikingo: Vikingo, unDragon: Dragon): Boolean = {
      unVikingo.peso >= pesoMaximo
    }
  }

  case class Jinete(unVikingo: Vikingo, unDragon: Dragon) extends Participante {

    override def cantidadDeCarga: Double = unDragon.peso / 5 + unDragon.danio

    override def danio: Double = unVikingo.danio + unDragon.danio

    def velocidad: Double = unDragon.velocidadDeVuelo - unVikingo.peso

    override def barbarosidad: Int = unVikingo.barbarosidad

    override def tieneArma: Boolean = unVikingo.item match {

      case Some(Arma(_)) => true
      case _ => false

    }

    override def  velocidad : Int =unDragon.velocidadDeVuelo-unVikingo.peso

    override def aumentarHambrePorPosta(posta: Posta): Participante ={
      this.copy(unVikingo.modificarNivelHambre(unVikingo.nivelHambre+5));
    }

    override def nivelHambre: Int = this.unVikingo.nivelHambre



  }

  abstract class Posta(){
    def puedeParticipar (participante: Participante):Boolean
    def quienEsMejor(participante: Participante,otroParticipante: Participante):Boolean
    def cuantoHambreAumento(participante: Participante):Int


    def participarEnPosta(participantes: List[Participante]):List[Participante]={
      var losQuePueden=participantes.filter(participante=>participante.evaluoParticipacion(this))
      losQuePueden.map(participante=>participante.aumentarHambrePorPosta(this))
      return losQuePueden.sortBy(quienEsMejor(_,_))
    }
  }


  case class Pesca(pesoMinimoALevantar:Option[Int]) extends Posta{

    def override puedeParticipar(participante: Participante):Boolean={
      pesoMinimoALevantar match{
        case Some(pesoMinimo) => vikingo.peso>pesoMinimo
        case None => true
      }
    }

    override def quienEsMejor(participante: Participante, otroParticipante: Participante): Boolean = {
      if (participante.cantidadDeCarga>otroParticipante.cantidadDeCarga){
      return participante} else{
      return otroParticipante}
    }

    override def cuantoHambreAumento(participante: Participante): Int =
      participante match {
        case Jinete(_,_)=> return 5
        case Vikingo(_,_)=>return 5
      }
  }

  case class Combate(nivelDeBarbaridadMinimo:Int) extends Posta{
    override def puedeParticipar(participante: Participante): Boolean = {
      vikingo.barbarosidad>nivelDeBarbaridadMinimo || vikingo.tieneArma
    }

    override def quienEsMejor(participante: Participante, otroParticipante: Participante): Boolean = {
      if (participante.danio>otroparticipante.danio){
        return participante
      }else{
        return otroparticipante}
    }

    override def cuantoHambreAumento(participante: Participante): Int =
      participante match {
        case Jinete(_,_)=> return 5
        case Vikingo(_,_)=>return 10
      }
  }

  case class Carrera(cantidadDeKilometros:Int,requiereMontura:Boolean) extends Posta{
    override def puedeParticipar(unParticipante: Participante): Boolean ={
        if requiereMontura {
          unParticipante match
            case Jinete(_,_) => true
            case Vikingo(_,_)=> false
        }

    }

    override def quienEsMejor(participante: Participante, otroParticipante: Participante): Boolean = {
      if (participante.velocidad> otroParticipante.velocidad){
        return participante
      }else{
        return otroParticipante}
    }

    override def cuantoHambreAumento(participante: Participante): Int =
      participante match {
        case Jinete(_,_)=> return cantidadDeKilometros
        case Vikingo(_,_)=>return 5
      }
  }



}

