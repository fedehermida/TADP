import scala.util.Random

/**
  * Created by Julian on 22/06/2017.
  */
package object FestivalDelInvierno {

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////// J U G A D O R E S //////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Jugador {
    def peso:Double
    def danio:Double
    def velocidad:Double
    def barbarosidad: Double
    def tieneArma: Boolean
    def nivelHambre:Double
    def cantidadDeCarga: Double
    def aumentarHambrePorPosta(posta: Posta): Jugador
    def usarItem : Jugador
    def puedoCompetir : Boolean

    def esMejorQue(participante:Jugador,posta:Posta):Boolean={
      posta.soyMejorQue(this,participante)
    }

  }

  ///////////////////
  /// J I N E T E ///
  ///////////////////

  case class Jinete(unVikingo: Vikingo, unDragon: Dragon) extends Jugador {

    def usarItem : Jinete = copy(unVikingo = unVikingo.usarItem)

    override def puedoCompetir: Boolean = unVikingo.puedoCompetir

    def velocidad: Double = unDragon.velocidadDeVuelo - unVikingo.peso

    override def cantidadDeCarga: Double = unDragon.cargaMaxima - unVikingo.peso

    override def danio: Double = unVikingo.danio + unDragon.danio

    override def barbarosidad: Double = unVikingo.barbarosidad

    override def tieneArma: Boolean = unVikingo.item match {
      case Some(Arma(_)) => true
      case _ => false
    }

    override def peso: Double = this.unVikingo.caracteristica.peso

    override def aumentarHambrePorPosta(posta: Posta): Jugador = copy(unVikingo = unVikingo.modificarNivelHambre(unVikingo.nivelHambre + 5))

    override def nivelHambre:Double = this.unVikingo.nivelHambre

  }

  ///////////////////
  // V I K I N G O //
  ///////////////////

  type CondicionParaCompetir = (Vikingo => Boolean)

  case class Vikingo(caracteristica: CaracteristicasVikingo, item: Option[Item], condicionesParaCompetir : List[CondicionParaCompetir]) extends Jugador {

    override def peso: Double = caracteristica.peso

    override def barbarosidad: Double = caracteristica.barbarosidad

    override def velocidad: Double = caracteristica.velocidad

    override def nivelHambre: Double = caracteristica.nivelHambre

    override def cantidadDeCarga: Double = peso / 2 + barbarosidad * 2

    def puedoCompetir: Boolean = condicionesParaCompetir.foldLeft(true)((semilla, condicion) => semilla & condicion(this))

    def usarItem: Vikingo = {
      item.foldLeft(this)((_,unItem) => unItem.aplicarEfecto(this))
    }

    def modificarNivelHambre(nuevoHambre: Double): Vikingo = copy(caracteristica = caracteristica.copy(nivelHambre = nuevoHambre))

    override def danio: Double = {
      item match {
        case Some(Arma(unDanio)) => barbarosidad + unDanio
        case _ => barbarosidad
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
      posta match {
        case Pesca(_) => this.copy(caracteristica = caracteristica.copy(nivelHambre = nivelHambre + 5))
        case Combate(_) => this.copy(caracteristica = caracteristica.copy(nivelHambre = nivelHambre + 10))
        case Carrera(cantidadDeKilometros, _) => this.copy(caracteristica = caracteristica.copy(nivelHambre = nivelHambre + cantidadDeKilometros))
      }
    }


    def puedoMontarlo(unDragon: Dragon): Boolean = {
      unDragon.puedoSerMontadoPor(this)
    }


    def participarEnMiMejorFormaEnUnaPosta(posta: Posta,dragones : List[Dragon]) : Option[Jugador] = {
      (this :: dragones.flatMap(montar(_))).filter(posta.puedeParticipar(_))
        .sortWith(posta.soyMejorQue)
        .headOption
    }

  }

  ///////////////////////////////////////////////////////////
  // C A R A C T E R I S T I C A S   D E L   V I K I N G O //
  ///////////////////////////////////////////////////////////

  case class CaracteristicasVikingo(peso: Double, velocidad: Double, barbarosidad: Double, nivelHambre: Double)

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////// I T E M S /////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  trait Item{
    def aplicarEfecto(vikingo: Vikingo): Vikingo ={
      vikingo
    }
  }

  case class Arma(unDanio: Int) extends Item

  case object SistemaDeVuelo extends Item

  case class Comestible(disminuirHambre: Double) extends Item{
    override def aplicarEfecto(vikingo: Vikingo): Vikingo = vikingo.modificarNivelHambre(0.0.max(vikingo.nivelHambre - disminuirHambre))
  }




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
    override def danio :Double = 150
  }

  /////////////////////////////////////
  ///////// G R O N C K L E ///////////
  /////////////////////////////////////

  case class Gronckle(velocidadBase: Double = 60,
                      peso: Double,
                      condicionesDeMonturaDragon: List[CondicionMontura]) extends Dragon{
    override def danio : Double = peso*5
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
    override def cumpleRequisito(unVikingo: Vikingo, unDragon: Dragon): Boolean = unVikingo.item.contains(unItem)
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

    def puedeParticipar (jugador: Jugador):Boolean = {
      jugador.nivelHambre + this.cuantoHambreAumento(jugador)  <= 100
    }

    def quienEsMejor(jugador: Jugador, otroJugador : Jugador) : Jugador = {
      if(soyMejorQue(jugador,otroJugador)){
        jugador
      }else{
        otroJugador
      }
    }

    def participarEnPosta(jugadores: List[Jugador]):List[Jugador]={
      var jugadoresLuegoDeCompetir = jugadores.map(_.aumentarHambrePorPosta(this))
      jugadoresLuegoDeCompetir.sortWith(soyMejorQue).map(_.usarItem)
    }
  }


  ///////////////////
  //// P E S C A ////
  ///////////////////

  case class Pesca(pesoMinimoALevantar:Option[Int]) extends Posta{

    override def puedeParticipar(jugador:Jugador) :Boolean={
      super.puedeParticipar(jugador) &&
        pesoMinimoALevantar.fold(true)(pesoMinimo => jugador.peso > pesoMinimo)
    }

    override def soyMejorQue(jugador: Jugador, otroJugador : Jugador): Boolean = {
      jugador.cantidadDeCarga > otroJugador.cantidadDeCarga
    }

    override def cuantoHambreAumento(participante: Jugador): Int = 5
  }

  ///////////////////
  // C O M B A T E //
  ///////////////////

  case class Combate(nivelDeBarbaridadMinimo:Int) extends Posta{
    override def puedeParticipar(jugador: Jugador): Boolean = {
      super.puedeParticipar(jugador) && jugador.barbarosidad>nivelDeBarbaridadMinimo || jugador.tieneArma
    }


    override def soyMejorQue(jugador: Jugador, otroJugador: Jugador): Boolean = {
      jugador.danio > otroJugador.danio
    }

    override def cuantoHambreAumento(jugador: Jugador): Int =
      jugador match {
        case Jinete(_,_)=> 5
        case Vikingo(_,_,_)=> 10
      }
  }


  ///////////////////
  // C A R R E R A //
  ///////////////////


  case class Carrera(cantidadDeKilometros:Int,requiereMontura:Boolean) extends Posta{

    override def puedeParticipar(jugador: Jugador): Boolean = {
      jugador match {
        case Jinete(_, _) => super.puedeParticipar(jugador)
        case Vikingo(_, _, _) => (!requiereMontura) & super.puedeParticipar(jugador)
      }
    }

    override def soyMejorQue(jugador : Jugador, otroJugador: Jugador): Boolean = {
      jugador.velocidad > otroJugador.velocidad
    }

    override def cuantoHambreAumento(jugador: Jugador): Int ={
      jugador match {
        case Jinete(_,_) => 5
        case Vikingo(_,_,_) => cantidadDeKilometros
      }
    }

  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////// T O R N E O ///////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  case class Torneo(dragones : List[Dragon], postas : List[Posta]) {

    def jugar(vikingos : List[Vikingo], regla : ReglasEstandar) : Option[Vikingo] = {
      val finalistas : List[Vikingo] = postas.foldLeft(vikingos)((semilla : List[Vikingo], posta : Posta) =>
        jugarRonda(posta,semilla,dragones,regla) match {
          case (_, 0) => return None
          case (jugadores, 1) => return jugadores.headOption
          case (jugadores,_)  => jugadores
        }
      )
      regla.obtenerGanador(finalistas)
    }

    def jugar(equipos : List[List[Vikingo]], regla : ReglasPorEquipos): Option[List[Vikingo]] ={
      val finalistas : List[List[Vikingo]] = postas.foldLeft(equipos)((semilla : List[List[Vikingo]], posta : Posta) =>
        jugarRondaConEquipos(posta,semilla,dragones,regla) match {
          case (_, 0) => return None
          case (jugadores, 1) => return jugadores.headOption
          case (jugadores,_)  => jugadores
        }
      )
      regla.obtenerGanador(finalistas)
    }

    def reagruparEquipos(unosEquipos : List[List[Vikingo]], unosVikingos : List[Vikingo]) : List[List[Vikingo]] = {
      val equipos = unosEquipos.foldLeft(List() : List[List[Vikingo]])((semilla : List[List[Vikingo]], equipo : List[Vikingo]) =>
        (semilla :+ (equipo.filter(unVikingo => unosVikingos.contains(unVikingo))))
      )
      equipos.filter(_.nonEmpty)
    }


    def obtenerVikingosDespuesDeLaPosta(jugadores : List[Jugador]) : List[Vikingo] = {
      jugadores.map(this.obtenerVikingo)
    }

    def obtenerVikingo(participante : Jugador) : Vikingo = {
      participante match{
        case participante : Jinete => participante.unVikingo
        case participante : Vikingo => participante
      }
    }

    def jugarRondaConEquipos(unaPosta : Posta, unosEquipos : List[List[Vikingo]], unosDragones : List[Dragon], regla : ReglasPorEquipos) : (List[List[Vikingo]], Int) ={
      val finalistas = reagruparEquipos(unosEquipos,
        obtenerVikingosDespuesDeLaPosta(
          regla.quienesPasanALaSiguienteRonda(
            unaPosta.participarEnPosta(regla.prepararParaUnaPosta(unaPosta, unosEquipos.flatten, unosDragones))
          )
        )
      )
      (finalistas,finalistas.length)
    }

    def jugarRonda(unaPosta : Posta, unosVikingos : List[Vikingo], unosDragones : List[Dragon], regla : ReglasEstandar): (List[Vikingo], Int) = {
      val finalistas = obtenerVikingosDespuesDeLaPosta(
        regla.quienesPasanALaSiguienteRonda(
          unaPosta.participarEnPosta(regla.prepararParaUnaPosta(unaPosta,unosVikingos,unosDragones))
        )
      )
      (finalistas,finalistas.length)
    }


  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////// R E G L A S ///////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Regla {

    var dragonesRestantes : List[Dragon] = List()

    def quienesPasanALaSiguienteRonda(participantes: List[Jugador]) : List[Jugador]

    def prepararParaUnaPosta(posta: Posta, vikingos: List[Vikingo], dragones : List[Dragon]): List[Jugador] = {
      dragonesRestantes = dragones
      vikingos.foldLeft(List() : List[Jugador])((semilla : List[Jugador], vikingo : Vikingo) =>
        this.prepararVikingoParaUnaPosta(vikingo,posta) match {
          case Some(jugador) => semilla :+ jugador
          case None => semilla
        }
      )
    }

    def prepararVikingoParaUnaPosta(unVikingo: Vikingo, unaPosta : Posta): Option[Jugador] ={
      val vikingoPreparado = unVikingo.participarEnMiMejorFormaEnUnaPosta(unaPosta,dragonesRestantes)
      vikingoPreparado match {
        case Some(Vikingo(_,_,_)) => ;
        case Some(Jinete(_,unDragon)) => dragonesRestantes = dragonesRestantes diff List(unDragon)
        case None => ;
      }
      vikingoPreparado
    }
  }


  /////////////////////////////////////
  /////// P O R   E Q U I P O S ///////
  /////////////////////////////////////

  case class ReglasPorEquipos() extends Regla{

    override def quienesPasanALaSiguienteRonda(jugadores: List[Jugador]): List[Jugador] = {
      jugadores.drop(jugadores.length / 2)
    }

    def obtenerGanador(equipos : List[List[Vikingo]]): Option[List[Vikingo]] = Random.shuffle(equipos).headOption

  }

  /////////////////////////////////////
  ///////// E S T A N D A R ///////////
  /////////////////////////////////////

  class ReglasEstandar extends Regla{


    override def quienesPasanALaSiguienteRonda(jugadores: List[Jugador]): List[Jugador] = {
      jugadores.take(jugadores.length / 2)
    }

    def obtenerGanador(vikingos: List[Vikingo]): Option[Vikingo] = vikingos.headOption

  }

  /////////////////////////////////////
  /// P O R   E L I M I N A C I O N ///
  /////////////////////////////////////

  case class ReglasDeEliminacion(numeroDeParticipantesEliminadosParaLaSiguienteRonda : Int) extends ReglasEstandar{
    require(numeroDeParticipantesEliminadosParaLaSiguienteRonda>=0)

    override def quienesPasanALaSiguienteRonda(jugadores : List[Jugador]) : List[Jugador] = {
      jugadores.reverse.drop(numeroDeParticipantesEliminadosParaLaSiguienteRonda).reverse
    }
  }

  /////////////////////////////////////
  //// T O R N E O   I N V E R S O ////
  /////////////////////////////////////

  case object ReglasDeTorneoInverso extends ReglasEstandar{

    override def quienesPasanALaSiguienteRonda(jugadores: List[Jugador]): List[Jugador] = {
      jugadores.drop(jugadores.length / 2)
    }

    override def obtenerGanador(vikingos : List[Vikingo]): Option[Vikingo] = {
      vikingos.reverse.headOption
    }
  }

  /////////////////////////////////////
  ///////// P O R   V E T O ///////////
  /////////////////////////////////////

  case class ReglasConVeto(condicionParaLosDragones : CondicionDragon) extends ReglasEstandar{


    override def prepararParaUnaPosta(posta: Posta, vikingos: List[Vikingo], dragones: List[Dragon]): List[Jugador] = {
      val dragonesQueCumplenCondicion = dragones.filter(condicionParaLosDragones(_))
      super.prepararParaUnaPosta(posta,vikingos,dragonesQueCumplenCondicion)
    }
  }

  /////////////////////////////////////
  ////// C O N   H A N D I C A P //////
  /////////////////////////////////////

  case object ReglasConHandicap extends ReglasEstandar{


    override def prepararParaUnaPosta(posta: Posta, vikingos: List[Vikingo], dragones: List[Dragon]): List[Jugador] = {
      super.prepararParaUnaPosta(posta, vikingos.reverse, dragones)
    }
  }

  type CondicionDragon = Dragon => Boolean


}
