import scala.collection.mutable
import scala.util.Random

/**
  * Created by TYPE Null on 9/6/2017.
  */
package object FestivalDelInvierno {

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////// P A R T I C I P A N T E S //////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Participante

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////// E Q U I P O ////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  class EquipoVikingo(var vikingos : List[Vikingo] = List()) extends Participante{
    def reagruparEquipo(unosVikingos : List[Vikingo]) : Unit = {
      vikingos = vikingos diff unosVikingos
    }

    def agregarVikingo(vikingo : Vikingo) : Unit = {
      vikingos = (vikingo :: vikingos)
    }
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////// J U G A D O R E S //////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Jugador extends Participante{
    def peso:Int
    def danio:Int
    def equipo:Option[EquipoVikingo]
    def velocidad:Int
    def barbarosidad: Int
    def tieneArma: Boolean
    def nivelHambre:Double
    def cantidadDeCarga: Double
    def aumentarHambrePorPosta(posta: Posta): Jugador

    def esMejorQue(participante:Jugador,posta:Posta):Boolean={
      posta.soyMejorQue(this,participante)
    }

  }

  ///////////////////
  /// J I N E T E ///
  ///////////////////

  case class Jinete(unVikingo: Vikingo, unDragon: Dragon) extends Jugador {

    def velocidad: Double = unDragon.velocidadDeVuelo - unVikingo.peso

    override def cantidadDeCarga: Double = unDragon.cargaMaxima - unVikingo.peso

    override def danio: Double = unVikingo.danio + unDragon.danio

    override def barbarosidad: Int = unVikingo.barbarosidad

    override def tieneArma: Boolean = unVikingo.item match {
      case Some(Arma(_)) => true
      case _ => false
    }

    override def peso: Int = this.unVikingo.caracteristica.peso

    override def aumentarHambrePorPosta(posta: Posta): Jugador = copy(unVikingo = unVikingo.modificarNivelHambre(unVikingo.nivelHambre + 5))

    override def nivelHambre:Double = this.unVikingo.nivelHambre

    override def equipo: Option[EquipoVikingo] = unVikingo.equipo
  }

  ///////////////////
  // V I K I N G O //
  ///////////////////

  case class Vikingo(caracteristica: CaracteristicasVikingo, item: Option[Item], equipo : Option[EquipoVikingo]) extends Jugador {

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


    override def aumentarHambrePorPosta(posta: Posta): Jugador = {
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


    def participarEnMiMejorFormaEnUnaPosta(posta: Posta, dragones : List[Dragon]) : Option[Jugador] = {
      var dragonesQuePuedoMontar : List[Dragon] = dragones.filter(_.puedoSerMontadoPor(this))
      if(!dragonesQuePuedoMontar.isEmpty){
        var participantesJinetes : List[Jinete] = dragonesQuePuedoMontar.map(this.montar(_).get)
        var posiblesMejoresParticipantes : List[Jugador] =  this :: participantesJinetes
        var mejorParticipante : Option[Jugador] = posiblesMejoresParticipantes.sortWith(posta.soyMejorQue(_,_)).filter(posta.puedeParticipar(_)).headOption
        return mejorParticipante
      }else{
        return List(this).filter(posta.puedeParticipar(_)).headOption
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
    def velocidadDeVuelo: Double = velocidadBase - peso
    def condicionesDeMonturaDragon:List[CondicionMontura]
    def velocidadBase:Double
    def cargaMaxima : Double = peso/5

    def puedoSerMontadoPor(vikingo : Vikingo): Boolean ={
      condicionesDeMonturaDragon.forall(condicion => condicion.cumpleRequisito(vikingo, this))
    }


  }

  /////////////////////////////////////
  //// F U R I A   N O C T U R N A ////
  /////////////////////////////////////

  case class FuriaNocturna(velocidadBase : Double = 60,
                           peso: Double,
                           danio: Double,
                           condicionesDeMonturaDragon: List[CondicionMontura]) extends Dragon{
    override def velocidadDeVuelo : Double = super.velocidadDeVuelo * 3
  }

  /////////////////////////////////////
  // N A D D E R   M O R T I F A G O //
  /////////////////////////////////////

  case class NadderMortifero(velocidadBase: Double = 60,
                             peso: Double,
                             condicionesDeMonturaDragon: List[CondicionMontura]) extends Dragon{
    override def danio :Double =return 150
  }

  /////////////////////////////////////
  ///////// G R O N C K L E ///////////
  /////////////////////////////////////

  case class Gronckle(velocidadBase: Double = 60,
                      peso: Double,
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
      unVikingo.peso <= unDragon.cargaMaxima
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


    def cuantoHambreAumento(participante: Jugador):Int
    def soyMejorQue(participante: Jugador,otroParticipante: Jugador):Boolean

    def puedeParticipar (participante: Jugador):Boolean = {
      participante.nivelHambre + this.cuantoHambreAumento(participante)  <= 100
    }

    def quienEsMejor(participante: Jugador, otroParticipante : Jugador) : Jugador = {
      if(soyMejorQue(participante,otroParticipante)){
        participante
      }else{
        otroParticipante
      }
    }

    def participarEnPosta(participantes: List[Jugador]):List[Jugador]={
      var participantesLuegoDeCompetir = participantes.map(_.aumentarHambrePorPosta(this))
      return participantesLuegoDeCompetir.sortWith(soyMejorQue(_,_))
    }
  }


  ///////////////////
  //// P E S C A ////
  ///////////////////

  case class Pesca(pesoMinimoALevantar:Option[Int]) extends Posta{

    override def puedeParticipar(participante:Jugador) :Boolean={
      if(super.puedeParticipar(participante)){
        pesoMinimoALevantar match{
          case Some(pesoMinimo) => participante.peso > pesoMinimo
          case None => true
        }
      }else{
        false
      }
    }

    override def soyMejorQue(participante: Jugador, otroParticipante: Jugador): Boolean = {
      participante.cantidadDeCarga > otroParticipante.cantidadDeCarga
    }

    override def cuantoHambreAumento(participante: Jugador): Int = 5
  }

  ///////////////////
  // C O M B A T E //
  ///////////////////

  case class Combate(nivelDeBarbaridadMinimo:Int) extends Posta{
    override def puedeParticipar(participante: Jugador): Boolean = {
      super.puedeParticipar(participante) && participante.barbarosidad>nivelDeBarbaridadMinimo || participante.tieneArma
    }


    override def soyMejorQue(participante: Jugador, otroParticipante: Jugador): Boolean = {
      participante.danio > otroParticipante.danio
    }

    override def cuantoHambreAumento(participante: Jugador): Int =
      participante match {
        case Jinete(_,_)=> 5
        case Vikingo(_,_,_)=> 10
      }
  }


  ///////////////////
  // C A R R E R A //
  ///////////////////


  case class Carrera(cantidadDeKilometros:Int,requiereMontura:Boolean) extends Posta{

    override def puedeParticipar(unParticipante: Jugador): Boolean ={

      if(super.puedeParticipar(unParticipante)) {
        if (requiereMontura) {
          unParticipante match {
            case Jinete(_, _) => true
            case Vikingo(_,_,_) => false
          }
        }else{
          true
        }
      }else{
        false
      }
    }

    override def soyMejorQue(participante: Jugador, otroParticipante: Jugador): Boolean = {
      participante.velocidad > otroParticipante.velocidad
    }

    override def cuantoHambreAumento(participante: Jugador): Int ={
      participante match {
        case Jinete(_,_) => 5
        case Vikingo(_,_,_) => cantidadDeKilometros
      }
    }

  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////// T O R N E O ///////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  case class Torneo(participantes : List[Participante], dragones : List[Dragon], postas : List[Posta], regla : Regla) {
    require((regla == ReglasPorEquipos & participantes.isInstanceOf[List[EquipoVikingo]]) | (regla != ReglasPorEquipos & participantes.isInstanceOf[List[Vikingo]]))

    def jugar() : Option[Participante] = {
      participantes match{
        case participantes : List[EquipoVikingo] => jugarTorneoConEquipos(participantes)
        case participantes : List[Vikingo] => jugarTorneoSinEquipos(participantes)
      }
    }

    def jugarTorneoSinEquipos(vikingos : List[Vikingo]) : Option[Jugador] = {
      var vikingosRestantes : List[Vikingo] = vikingos
      for (posta <- postas) {
        vikingosRestantes.length match{
          case 0 => return None
          case 1 => return vikingosRestantes.headOption
          case _ => vikingosRestantes = this.obtenerVikingosDespuesDeLaPosta(
            regla.quienesPasanALaSiguienteRonda(
              posta.participarEnPosta(regla.prepararParaLaPosta(posta,vikingos,dragones))
            )
          )
        }
      }

      regla.obtenerGanador(vikingosRestantes)
    }

    def jugarTorneoConEquipos(equipos : List[EquipoVikingo]) : Option[Participante] = {
      var equiposRestantes : List[EquipoVikingo] = equipos

      for (posta <- postas) {
        equiposRestantes.length match{
          case 0 => return None
          case 1 => return equiposRestantes.headOption
          case _ => equiposRestantes = this.reagruparEquipos(
            obtenerVikingosDespuesDeLaPosta(
              regla.quienesPasanALaSiguienteRonda(
                posta.participarEnPosta(regla.prepararParaLaPosta(posta,equiposRestantes.flatMap(_.vikingos),dragones))
              )
            )
          )
        }
      }

      regla.obtenerGanador(equiposRestantes.map(_.vikingos.head))
    }

    def reagruparEquipos(unosVikingos : List[Vikingo]): List[EquipoVikingo] ={
      var vikingosRestantes : List[Vikingo] = unosVikingos
      var equiposRestantes : List[EquipoVikingo] = List()
      var equipo : EquipoVikingo = new EquipoVikingo()
      for(vikingo <- vikingosRestantes){
        equipo = vikingo.equipo.get
        equipo = new EquipoVikingo(equipo.vikingos diff unosVikingos)
        vikingosRestantes = vikingosRestantes diff equipo.vikingos
        equiposRestantes = equipo :: equiposRestantes
      }
      equiposRestantes
    }

    def obtenerVikingosDespuesDeLaPosta(jugadores : List[Jugador]) : List[Vikingo] = {
      return jugadores.map(this.obtenerVikingo(_))
    }

    def obtenerVikingo(participante : Jugador) : Vikingo = {
      participante match{
        case participante : Jinete => participante.unVikingo
        case participante : Vikingo => participante
      }
    }
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////// R E G L A S ///////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Regla {
    def prepararParaLaPosta(posta: Posta, vikingos: List[Vikingo], dragones : List[Dragon]): List[Jugador] = {
      var dragonesRestantes : List[Dragon] = dragones
      val participantes : List[Option[Jugador]] = for {
        vikingo <- vikingos
      } yield {
        var participanteListo : Option[Jugador] = vikingo.participarEnMiMejorFormaEnUnaPosta(posta, dragonesRestantes)
        var dragonElegido : Option[Dragon] = participanteListo match{
          case None  => None
          case Some(Jinete(_,unDragon)) => Option(unDragon)
          case Some(Vikingo(_,_,_)) => None
        }
        dragonesRestantes = dragonElegido match {
          case Some(dragon) => dragonesRestantes diff List(dragon)
          case None => dragonesRestantes
        }
        vikingo.participarEnMiMejorFormaEnUnaPosta(posta, dragonesRestantes)
      }
      return participantes.filter(_.isEmpty).map(_.get)
    }

    def quienesPasanALaSiguienteRonda(participantes: List[Jugador]) : List[Jugador]
    def obtenerGanador(participantes : List[Jugador]) : Option[Jugador]
  }

  case object ReglasPorEquipos extends Regla{

    override def quienesPasanALaSiguienteRonda(participantes: List[Jugador]): List[Jugador] = {
      participantes.drop(participantes.length / 2)
    }

    override def obtenerGanador(participantes: List[Jugador]): Option[Jugador] = Random.shuffle(participantes).headOption

  }

  case class ReglasEstandar() extends Regla{

    override def quienesPasanALaSiguienteRonda(participantes: List[Jugador]): List[Jugador] = {
      participantes.take(participantes.length / 2)
    }

    override def obtenerGanador(participantes: List[Jugador]): Option[Jugador] = participantes.headOption

  }

  case class ReglasDeEliminacion(numeroDeParticipantesEliminadosParaLaSiguienteRonda : Int) extends ReglasEstandar{
    require(numeroDeParticipantesEliminadosParaLaSiguienteRonda>=0)
    override def quienesPasanALaSiguienteRonda(participantes : List[Jugador]) : List[Jugador] = {
      participantes.reverse.drop(numeroDeParticipantesEliminadosParaLaSiguienteRonda).reverse
    }
  }

  case object ReglasDeTorneoInverso extends ReglasEstandar{
    override def quienesPasanALaSiguienteRonda(participantes: List[Jugador]): List[Jugador] = {
      participantes.drop(participantes.length / 2)
    }

    override def obtenerGanador(participantes: List[Jugador]): Option[Jugador] = {
      participantes.reverse.headOption
    }
  }

  case class ReglasConVeto(condicionParaLosDragones : CondicionDragon) extends ReglasEstandar{
    override def prepararParaLaPosta(posta: Posta, vikingos: List[Vikingo], dragones: List[Dragon]): List[Jugador] = {
      val dragonesQueCumplenCondicion = dragones.filter(condicionParaLosDragones(_))
      super.prepararParaLaPosta(posta,vikingos,dragones)
    }
  }

  case object ReglasConHandicap extends ReglasEstandar{
    override def prepararParaLaPosta(posta: Posta, vikingos: List[Vikingo], dragones: List[Dragon]): List[Jugador] = {
      super.prepararParaLaPosta(posta, vikingos.reverse, dragones)
    }
  }

  type CondicionDragon = Dragon => Boolean


}