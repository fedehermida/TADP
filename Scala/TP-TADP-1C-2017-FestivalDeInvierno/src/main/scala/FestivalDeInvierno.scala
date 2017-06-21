import scala.collection.mutable

/**
  * Created by TYPE Null on 9/6/2017.
  */
package object FestivalDelInvierno {

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////// P A R T I C I P A N T E S //////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Participante {
    def peso:Double
    def danio:Double
    def velocidad:Double
    def barbarosidad: Double
    def tieneArma: Boolean
    def nivelHambre:Double
    def cantidadDeCarga: Double
    def aumentarHambrePorPosta(posta: Posta): Participante

    def esMejorQue(participante:Participante,posta:Posta):Boolean={
      posta.soyMejorQue(this,participante)
    }

  }
  ///////////////////
  /// E Q U I P O ///
  ///////////////////

  /*case class Equipo(nombre:String,miembros:List[Vikingo]) extends Participante{

    def pedirIntegrantesConIdentifiacion():List[(Vikingo,String)]={
      return miembros.map(vikingo=>prepararTuplaParaParticipar(vikingo,nombre))
    }

    def prepararTuplaParaParticipar(vikingo: Vikingo,nombreEquipo: String): (Vikingo,String) ={
      return (vikingo,nombreEquipo)
    }

  }*/

  ///////////////////
  /// J I N E T E ///
  ///////////////////

  case class Jinete(unVikingo: Vikingo, unDragon: Dragon) extends Participante {

    def velocidad: Double = unDragon.velocidadDeVuelo-unVikingo.peso

    override def cantidadDeCarga: Double = unDragon.peso / 5 + unDragon.danio

    override def danio: Double = unVikingo.danio + unDragon.danio

    override def barbarosidad: Double = unVikingo.barbarosidad

    override def tieneArma: Boolean = unVikingo.item match {
      case Some(Arma(_)) => true
      case _ => false
    }

    override def peso: Double = this.unVikingo.caracteristica.peso

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

  case class RequisitoItem(unItem: Option[Item]) extends CondicionMontura {
    override def cumpleRequisito(unVikingo: Vikingo, unDragon: Dragon): Boolean = {
      unVikingo.item.equals(unItem)
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

  case class Torneo(vikingos : List[Vikingo], dragones : List[Dragon], postas : List[Posta], regla : Regla){

    def jugar() : Option[Participante] = {
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

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////// R E G L A S ///////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Regla {
    def prepararParaLaPosta(posta : Posta, vikingos : List[Vikingo], dragones : List[Dragon]) : List[Participante]
    def quienesPasanALaSiguienteRonda(participantes: List[Participante]) : List[Participante]
    def obtenerGanador(participantes : List[Participante]) : Option[Participante]
  }

  /////////////////////
  // E S T A N D A R //
  /////////////////////

  abstract class ReglasEstandar() extends Regla{

    override def prepararParaLaPosta(posta: Posta, vikingos: List[Vikingo], dragones : List[Dragon]): List[Participante] = {
      var dragonesRestantes : List[Dragon] = dragones
      for {
        vikingo <- vikingos
      } yield {
        var participanteListo = vikingo.participarEnMiMejorFormaEnUnaPosta(posta, dragonesRestantes)
        var dragonElegido = participanteListo match{
          case participanteListo : Jinete => Option(participanteListo.unDragon)
          case participanteListo : Vikingo => None
        }
        dragonesRestantes = dragonElegido match {
          case Some(dragon) => dragonesRestantes diff List(dragon)
          case None => dragonesRestantes
        }
        vikingo.participarEnMiMejorFormaEnUnaPosta(posta, dragonesRestantes)
      }
    }

    override def quienesPasanALaSiguienteRonda(participantes: List[Participante]): List[Participante] = {
      participantes.take(participantes.length / 2)
    }

    override def obtenerGanador(participantes: List[Participante]): Option[Participante] = participantes.headOption

  }

  ///////////////////////////////////
  // P O R   E L I M I N A C I O N //
  ///////////////////////////////////

  case class ReglasDeEliminacion(numeroDeParticipantesEliminadosParaLaSiguienteRonda : Int) extends ReglasEstandar{
    require(numeroDeParticipantesEliminadosParaLaSiguienteRonda>=0)
    override def quienesPasanALaSiguienteRonda(participantes : List[Participante]) : List[Participante] = {
      participantes.reverse.drop(numeroDeParticipantesEliminadosParaLaSiguienteRonda).reverse
    }
  }

  /////////////////////////////////
  // T O R N E O   I N V E R S O //
  /////////////////////////////////

  case object ReglasDeTorneoInverso extends ReglasEstandar{
    override def quienesPasanALaSiguienteRonda(participantes: List[Participante]): List[Participante] = {
      participantes.drop(participantes.length / 2)
    }

    override def obtenerGanador(participantes: List[Participante]): Option[Participante] = {
      participantes.reverse.headOption
    }
  }

  /////////////////////
  // C O N   V E T O //
  /////////////////////

  case class ReglasConVeto(condicionParaLosDragones : CondicionDragon) extends ReglasEstandar{
    override def prepararParaLaPosta(posta: Posta, vikingos: List[Vikingo], dragones: List[Dragon]): List[Participante] = {
      val dragonesQueCumplenCondicion = dragones.filter(condicionParaLosDragones(_))
      super.prepararParaLaPosta(posta,vikingos,dragones)
    }
  }

  /////////////////////////////
  // C O N   H A N D I C A P //
  /////////////////////////////

  case object ReglasConHandicap extends ReglasEstandar{
    override def prepararParaLaPosta(posta: Posta, vikingos: List[Vikingo], dragones: List[Dragon]): List[Participante] = {
      super.prepararParaLaPosta(posta, vikingos.reverse, dragones)
    }
  }

  type CondicionDragon = Dragon => Boolean

  //case object ReglaPorEquipos extends Regla

}
