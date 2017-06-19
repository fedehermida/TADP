import scala.collection.mutable

/**
  * Created by TYPE Null on 9/6/2017.
  */
object FestivalDelInvierno {

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////// P A R T I C I P A N T E S //////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Participante {
    def peso:Int
    def danio:Int
    def velocidad:Int
    def barbarosidad: Int
    def tieneArma: Boolean
    def nivelHambre:Double
    def cantidadDeCarga: Double
    def aumentarHambrePorPosta(posta: Posta): Participante

    def esMejorQue(participante:Participante,posta:Posta):Boolean={
      posta.soyMejorQue(this,participante)
    }

  }

  ///////////////////
  /// J I N E T E ///
  ///////////////////

  case class Jinete(unVikingo: Vikingo, unDragon: Dragon) extends Participante {

    def velocidad: Double = unDragon.velocidadDeVuelo - unVikingo.peso

    override def cantidadDeCarga: Double = unDragon.peso / 5 + unDragon.danio

    override def danio: Double = unVikingo.danio + unDragon.danio

    override def barbarosidad: Int = unVikingo.barbarosidad

    override def tieneArma: Boolean = unVikingo.item match {
      case Some(Arma(_)) => true
      case _ => false
    }

    override def peso: Int = this.unVikingo.caracteristica.peso

    override def aumentarHambrePorPosta(posta: Posta): Participante ={
      this.copy(unVikingo.modificarNivelHambre(unVikingo.nivelHambre+5));
    }

    override def nivelHambre:Double = this.unVikingo.nivelHambre

  }

  ///////////////////
  // V I K I N G O //
  ///////////////////

  case class Vikingo(caracteristica: CaracteristicasVikingo, item: Option[Item]) extends Participante {

    override def peso = caracteristica.peso

    override def barbarosidad = caracteristica.barbarosidad

    override def velocidad = caracteristica.velocidad

    override def nivelHambre = caracteristica.nivelHambre

    override def cantidadDeCarga: Double = peso / 2 + barbarosidad * 2

    def modificarNivelHambre(nuevoHambre: Double) :Vikingo = copy(caracteristica = caracteristica.copy(nivelHambre = nuevoHambre))

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


    def montar(unDragon: Dragon): Option[Jinete] = {
      if (puedoMontarlo(unDragon))
        Some(Jinete(this, unDragon))
      else
        None
    }


    override def aumentarHambrePorPosta(posta: Posta): Participante = {
      posta match
      {
        case Pesca(_) => this.copy(caracteristica = caracteristica.copy(nivelHambre=nivelHambre+5))
        case Combate(_) => this.copy(caracteristica = caracteristica.copy(nivelHambre=nivelHambre+10))
        case Carrera(cantidadDeKilometros, _) => this.copy(caracteristica = caracteristica.copy(nivelHambre=nivelHambre+cantidadDeKilometros))
      }
    }


    def puedoMontarlo(unDragon: Dragon): Boolean = {
      unDragon.puedoSerMontadoPor(this)
    }


    def participarEnMiMejorFormaEnUnaPosta(posta: Posta, dragones : List[Dragon]) : Participante = {
      var dragonesQuePuedoMontar : List[Dragon] = dragones.filter(_.puedoSerMontadoPor(this))

      if(!dragonesQuePuedoMontar.isEmpty){
        var participantesJinetes : List[Jinete] = dragonesQuePuedoMontar.map(this.montar(_).get)
        var mejorJinete : Jinete = participantesJinetes.sortWith(posta.soyMejorQue(_,_)).head
        return posta.quienEsMejor(mejorJinete, this)
      }else{
        return this
      }
    }

  }


  ///////////////////////////////////////////////////////////
  // C A R A C T E R I S T I C A S   D E L   V I K I N G O //
  ///////////////////////////////////////////////////////////

  case class CaracteristicasVikingo(peso: Int, velocidad: Int, barbarosidad: Int, nivelHambre: Double)

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////// I T E M S /////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  trait Item

  case class Arma(unDanio: Int) extends Item

  case object SistemaDeVuelo extends Item

  case class Comestible(disminuirHambre: Double) extends Item


  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////// D R A G O N E S /////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  trait Dragon{

    def danio:Double
    def peso:Double
    def velocidadDeVuelo: Double
    def condicionesDeMonturaDragon:List[CondicionMontura]
    def velocidadBase:Double

    def puedoSerMontadoPor(vikingo : Vikingo): Boolean ={
      condicionesDeMonturaDragon.forall(condicion => condicion.cumpleRequisito(vikingo, this))
    }

  }

  /////////////////////////////////////
  //// F U R I A   N O C T U R N A ////
  /////////////////////////////////////

  case class FuriaNocturna(peso: Double,
                           velocidadDeVuelo: Double,
                           danio: Double,
                           cargaMaxima: Double,
                           cargaActual: Double,
                           condicionesDeMonturaDragon: List[CondicionMontura]) extends Dragon{
    def velocidadBase= this.velocidadBase * 3
  }

  /////////////////////////////////////
  // N A D D E R   M O R T I F A G O //
  /////////////////////////////////////

  case class NadderMortifero(velocidadBase: Double,
                             peso: Double,
                             velocidadDeVuelo: Double,
                             cargaMaxima: Double,
                             cargaActual: Double,
                             condicionesDeMonturaDragon: List[CondicionMontura]) extends Dragon{
    override def danio :Double =return 150
  }

  /////////////////////////////////////
  ///////// G R O N C K L E ///////////
  /////////////////////////////////////

  case class Gronckle(velocidadBase: Double,
                      peso: Double,
                      velocidadDeVuelo: Double,
                      cargaMaxima: Double,
                      cargaActual: Double,
                      condicionesDeMonturaDragon: List[CondicionMontura]) extends Dragon{
    override def danio= peso*5
  }


  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////// C O N D I C I O N E S   D E   M O N T U R A /////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  trait CondicionMontura {
    def cumpleRequisito(unVikingo: Vikingo, unDragon: Dragon): Boolean
  }


  case object RequisitoBasico extends CondicionMontura {
    override def cumpleRequisito(unVikingo: Vikingo, unDragon: Dragon): Boolean = {
      unVikingo.peso <= unDragon.peso/5
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



  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////// P O S T A S ///////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  trait Posta{


    def cuantoHambreAumento(participante: Participante):Int
    def soyMejorQue(participante: Participante,otroParticipante: Participante):Boolean

    def puedeParticipar (participante: Participante):Boolean = {
      participante.nivelHambre + this.cuantoHambreAumento(participante)  <= 100
    }

    def quienEsMejor(participante: Participante, otroParticipante : Participante) : Participante = {
      if(soyMejorQue(participante,otroParticipante)){
        participante
      }else{
        otroParticipante
      }
    }

    def participarEnPosta(participantes: List[Participante]):List[Participante]={
      var participantesQuePuedenCompetirEnLaPosta = participantes.filter(this.puedeParticipar(_))
      var participantesLuegoDeCompetir = participantesQuePuedenCompetirEnLaPosta.map(_.aumentarHambrePorPosta(this))
      return participantesLuegoDeCompetir.sortWith(soyMejorQue(_,_))
    }
  }


  ///////////////////
  //// P E S C A ////
  ///////////////////

  case class Pesca(pesoMinimoALevantar:Option[Int]) extends Posta{

    override def puedeParticipar(participante:Participante) :Boolean={
      if(super.puedeParticipar(participante)){
        pesoMinimoALevantar match{
          case Some(pesoMinimo) => participante.peso > pesoMinimo
          case None => true
        }
      }else{
        false
      }
    }

    override def soyMejorQue(participante: Participante, otroParticipante: Participante): Boolean = {
      participante.cantidadDeCarga > otroParticipante.cantidadDeCarga
    }

    override def cuantoHambreAumento(participante: Participante): Int = 5
  }

  ///////////////////
  // C O M B A T E //
  ///////////////////

  case class Combate(nivelDeBarbaridadMinimo:Int) extends Posta{
    override def puedeParticipar(participante: Participante): Boolean = {
      super.puedeParticipar(participante) && participante.barbarosidad>nivelDeBarbaridadMinimo || participante.tieneArma
    }


    override def soyMejorQue(participante: Participante, otroParticipante: Participante): Boolean = {
      participante.danio > otroParticipante.danio
    }

    override def cuantoHambreAumento(participante: Participante): Int =
      participante match {
        case Jinete(_,_)=> return 5
        case Vikingo(_,_)=>return 10
      }
  }


  ///////////////////
  // C A R R E R A //
  ///////////////////


  case class Carrera(cantidadDeKilometros:Int,requiereMontura:Boolean) extends Posta{

    override def puedeParticipar(unParticipante: Participante): Boolean ={

      if(super.puedeParticipar(unParticipante)) {
        if (requiereMontura) {
          unParticipante match {
            case Jinete(_, _) => true
            case Vikingo(_, _) => false
          }
        }else{
          true
        }
      }else{
        false
      }
    }

    override def soyMejorQue(participante: Participante, otroParticipante: Participante): Boolean = {
      participante.velocidad > otroParticipante.velocidad
    }

    override def cuantoHambreAumento(participante: Participante): Int ={
      participante match {
        case Jinete(_,_)=> return 5
        case Vikingo(_,_)=>return cantidadDeKilometros
      }
    }

  }


  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////// T O R N E O ///////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  case class Torneo(vikingos : List[Vikingo], dragones : List[Dragon], postas : List[Posta]){

    def jugar() : Option[Participante] = {
      var participantesPreparadosParaCompetirEnLaPosta : mutable.MutableList[Participante] = new mutable.MutableList[Participante]
      var vikingosRestantes : List[Vikingo] = vikingos

      for (posta <- postas) {
        if(!vikingosRestantes.isEmpty) {
          if(vikingosRestantes.length == 1){
            return vikingosRestantes.headOption
          }else{
            var dragonesTodavíaDisponibles: List[Dragon] = dragones
            for (vikingo <- vikingosRestantes) {
              var participantePreparadoParaLaPosta: Participante = vikingo.participarEnMiMejorFormaEnUnaPosta(posta, dragonesTodavíaDisponibles)
              participantesPreparadosParaCompetirEnLaPosta += participantePreparadoParaLaPosta
              dragonesTodavíaDisponibles = this.obtenerDragon(participantePreparadoParaLaPosta) match {
                case Some(dragon) => this.quitarDragonMontadoDeLaLista(dragonesTodavíaDisponibles, dragon)
                case None => dragonesTodavíaDisponibles
              }
            }
            var participantesRestantes = posta.participarEnPosta(participantesPreparadosParaCompetirEnLaPosta.toList)
            vikingosRestantes = this.obtenerVikingosDespuesDeLaPosta(participantesRestantes)
          }
        }else{
          return None
        }
      }

      return vikingosRestantes.headOption

    }

    def quitarDragonMontadoDeLaLista(unosDragones: List[Dragon], unDragon: Dragon): List[Dragon] = {
      unosDragones diff List(unDragon)
    }

    def obtenerVikingosDespuesDeLaPosta(participantes : List[Participante]) : List[Vikingo] = {
      return participantes.map(this.obtenerVikingo(_))
    }


    def obtenerVikingo(participante : Participante) : Vikingo = {
      participante match{
        case participante : Jinete => participante.unVikingo
        case participante : Vikingo => participante
      }
    }


    def obtenerDragon(participante : Participante) : Option[Dragon] = {
      participante match{
        case participante : Jinete => Option(participante.unDragon)
        case participante : Vikingo => None
      }
    }

  }

}

