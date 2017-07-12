import scala.collection.mutable.ListBuffer
import scala.util.Random


package object FestivalDeInvierno {

  trait Participante {
    def maximoKilosPescado: Double

    def barbarosidad: Double

    def tieneArma: Boolean

    def danio: Double

    def velocidad: Double

    def nivelDeHambre: Double

    def aumentarHambrePorPosta(unaPosta: Posta): Option[Participante]

    def esMejorQue(unParticipante: Participante, unaPosta: Posta): Boolean = {
      unaPosta match {
        case Pesca(_) => CriterioMejorPescador(this, unParticipante)
        case Combate(_) => CriterioMejorCombate(this, unParticipante)
        case Carrera(_, _) => CriterioMejorCarrera(this, unParticipante)
      }
    }

    def mejorEnPosta(unParticipante: Participante, unaPosta: Posta): Participante = {
      unaPosta match {
        case Pesca(_) => MejorPescador(this, unParticipante)
        case Combate(_) => MejorCombatiente(this, unParticipante)
        case Carrera(_, _) => MejorCorredor(this, unParticipante)
      }

    }


  }


  case class Vikingo(peso: Double,
                     velocidadBase: Double,
                     barbarosidad: Double,
                     fuerza: Double,
                     nivelDeHambre: Double,
                     item: Option[Item]) extends Participante {
    require(peso > 0, "El peso debe ser positivo")
    require(velocidadBase >= 0, "La velocidad debe ser positiva")
    require(barbarosidad >= 0, "Barbarosidad debe ser mayor que 0")
    require(fuerza >= 0, "La Fuerza debe ser mayor que 0")
    require(0 to 100 contains nivelDeHambre, "El nivel de hambre no puede superar el 100%")

    def montar(unDragon: Dragon): Option[Jinete] = {

      if (unDragon.puedeMontarme(this)) Some(Jinete(this, unDragon))
      else None

    }

    def aumentarHambre(unaCantidad: Double): Option[Vikingo] = {
      val hambriento: Boolean = nivelDeHambre + unaCantidad > 100
      hambriento match {
        case false => Some(this.copy(nivelDeHambre = nivelDeHambre + unaCantidad))
        case true => None
      }
    }

    override def aumentarHambrePorPosta(unaPosta: Posta): Option[Vikingo] = {
      unaPosta match {
        case Pesca(_) => aumentarHambre(5)
        case Combate(_) => aumentarHambre(10)
        case Carrera(_, cantKilometros) => aumentarHambre(cantKilometros)

      }
    }

    override def velocidad: Double = velocidadBase

    override def maximoKilosPescado: Double = peso * 0.5 + barbarosidad * 2

    override def danio = {
      this.item match {
        case Some(Arma(unDanio)) => fuerza + unDanio
        case _ => fuerza
      }
    }

    override def tieneArma: Boolean = {
      item match {
        case Some(Arma(_)) => true
        case _ => false
      }

    }


    /*
    val dragonesMontables: List[Dragon]  = dragones.filter(_.puedeMontarme(this))
    val posiblesJinetes: List[Jinete] = dragonesMontables.map(unDragon => montar(unDragon).get)
    val mejorJineteVikingo: List[Participante] = posiblesJinetes.map { unJinete =>

      unaPosta match {
        case Pesca(_) => MejorPescador(unJinete,this)
        case Combate(_) => MejorCombatiente(unJinete,this)
        case Carrera(_,_) => MejorCorredor(unJinete,this)
      }
    }
    val cumplenRequisito: List[Participante] = mejorJineteVikingo.filter(unaPosta.puedeParticipar(_))
    val mejorCompetidor: Participante = cumplenRequisito.reduceLeft{ (unCompetidor, otroCompetidor) =>
      unaPosta match {
        case Pesca(_) => MejorPescador(unCompetidor, otroCompetidor)
        case Combate(_) => MejorCombatiente(unCompetidor, otroCompetidor)
        case Carrera(_,_) => MejorCorredor(unCompetidor, otroCompetidor)
      }


    }
    mejorCompetidor
  }
*/

    def mejorMontura(dragones: List[Dragon], unaPosta: Posta): Option[Participante] = {

      val puedoParticiparComoVikingo: Boolean = unaPosta.puedeParticipar(this)
      val formasComoJinete: List[Participante] = for {
        dragon <- dragones
        jinete <- montar(dragon)

      } yield jinete

      val mejoresCombinaciones: List[Participante] = puedoParticiparComoVikingo match{
        case true => formasComoJinete :+ this
        case false => formasComoJinete
      }


      val mejorForma: Option[Participante] = mejoresCombinaciones.sortWith((participante, otro) => participante.esMejorQue(otro, unaPosta)).headOption
      mejorForma
    }
  }

  trait Item

  case class Arma(danio: Double) extends Item {
    require(danio >= 0, "El danio es siempre positivo")
  }

  case object SistemaDeVuelo extends Item

  case class Alimento(subirEnergia: Double) extends Item

  trait Dragon {
    def velocidad: Double

    def danio: Double

    def peso: Double

    def requisitos: List[Requisito]

    def puedeMontarme(unVikingo: Vikingo): Boolean = requisitos.forall(requisito => requisito.apply(unVikingo, this))

    def maximoKilosPescado: Double = this.peso * 0.2

  }

  case class FuriaNocturna(velocidadBase: Double = 60, danioBase: Double, pesoBase: Double, requisitos: List[Requisito]) extends Dragon {
    override def velocidad: Double = this.velocidadBase * 5

    override def danio: Double = this.danioBase

    override def peso: Double = this.pesoBase
  }

  case class Gronckle(velocidadBase: Double = 60, danioBase: Double, pesoBase: Double, requisitos: List[Requisito]) extends Dragon {
    override def velocidad: Double = velocidadBase / 2

    override def danio: Double = danioBase * 5

    override def peso: Double = pesoBase
  }

  case class NadderMortifero(velocidadBase: Double = 60, danioBase: Double = 150, pesoBase: Double, requisitos: List[Requisito]) extends Dragon {
    override def velocidad: Double = velocidadBase / 2

    override def danio: Double = 150

    override def peso: Double = pesoBase
  }

  trait Requisito {
    def apply(unVikingo: Vikingo, unDragon: Dragon): Boolean
  }

  case object RequisitoBase extends Requisito {
    override def apply(unVikingo: Vikingo, unDragon: Dragon): Boolean = {
      unDragon.peso * 0.2 > unVikingo.peso
    }
  }

  case class RequisitoBarbarosidad(minimoBarbarosidad: Double) extends Requisito {
    override def apply(unVikingo: Vikingo, unDragon: Dragon): Boolean = {
      unVikingo.barbarosidad > minimoBarbarosidad
    }
  }

  case class RequisitoObjeto(unObjeto: Item) extends Requisito {
    override def apply(unVikingo: Vikingo, unDragon: Dragon): Boolean = {
      unVikingo.item match {
        case Some(item) => item == unObjeto
        case None => false

      }
    }
  }

  case object RequisitoNadder extends Requisito {
    override def apply(unVikingo: Vikingo, unDragon: Dragon): Boolean = {
      unVikingo.fuerza < unDragon.danio
    }
  }

  case class RequisitoGronckle(pesoMaximo: Double) extends Requisito {
    override def apply(unVikingo: Vikingo, unDragon: Dragon): Boolean = {
      unVikingo.peso < pesoMaximo
    }
  }


  case class Jinete(unVikingo: Vikingo, unDragon: Dragon) extends Participante {
    override def maximoKilosPescado = unDragon.maximoKilosPescado - unVikingo.peso

    override def barbarosidad: Double = unVikingo.barbarosidad

    override def danio = unVikingo.fuerza + unDragon.danio

    override def velocidad = unDragon.velocidad - unVikingo.peso

    override def tieneArma: Boolean = unVikingo.tieneArma

    override def nivelDeHambre: Double = unVikingo.nivelDeHambre

    override def aumentarHambrePorPosta(unaPosta: Posta): Option[Participante] = {
      val vikingoEngordado: Option[Vikingo] = unVikingo.aumentarHambre(5)
      vikingoEngordado match {
        case Some (_) => Some(Jinete(vikingoEngordado.get,this.unDragon))
        case None => None
      }

    }
  }
  trait Posta {
    def puedeParticipar(unParticipante: Participante): Boolean

    def participar(participantes: List[Participante]): List[Participante] = {
      for {
        participante <- participantes
        if puedeParticipar(participante)
        unp <- participante.aumentarHambrePorPosta(this)
      } yield unp

    }
  }

  case class Pesca(pesoMinimo: Option[Double]) extends Posta {
    override def puedeParticipar(unParticipante: Participante): Boolean = {
      val reqBasico: Boolean = RequisitoBasicoPosta(unParticipante, this)
      this.pesoMinimo match {
        case Some(num) => (unParticipante.maximoKilosPescado > num) && reqBasico
        case _ => true && reqBasico
      }
    }

    override def participar(participantes: List[Participante]): List[Participante] = {
      val participantesHabilitados = super.participar(participantes)
      val mejoresParticipantes = participantesHabilitados.sortWith(CriterioMejorPescador(_, _))
      participantesHabilitados
    }

  }

  object CriterioMejorPescador {
    def apply(unParticipante: Participante, otroParticipante: Participante): Boolean = unParticipante.maximoKilosPescado > otroParticipante.maximoKilosPescado
  }

  object MejorPescador {
    def apply(unParticipante: Participante, otroParticipante: Participante): Participante = {
      if (CriterioMejorPescador(unParticipante, otroParticipante)) unParticipante
      else otroParticipante
    }
  }

  case class Combate(barbarosidadMinima: Double) extends Posta {
    override def puedeParticipar(unParticipante: Participante): Boolean = {
      val reqBasico: Boolean = RequisitoBasicoPosta(unParticipante, this)
      val superaBarbarosidadMinima = unParticipante.barbarosidad > barbarosidadMinima
      val tieneArma = unParticipante.tieneArma
      val cumpleCondicion = (tieneArma | superaBarbarosidadMinima) && reqBasico
      cumpleCondicion
    }

    override def participar(participantes: List[Participante]): List[Participante] = {
      val participantesHabilitados = super.participar(participantes)
      val mejoresParticipantes = participantesHabilitados.sortWith(CriterioMejorCombate(_, _))
      return mejoresParticipantes
    }

  }

  object CriterioMejorCombate {
    def apply(unParticipante: Participante, otroParticipante: Participante): Boolean = {
      val mejorParticipante = unParticipante.danio > otroParticipante.danio
      mejorParticipante
    }
  }

  object MejorCombatiente {
    def apply(unParticipante: Participante, otroParticipante: Participante): Participante = {

      CriterioMejorCombate(unParticipante, otroParticipante) match {
        case true => unParticipante
        case _ => otroParticipante
      }
    }
  }

  case class Carrera(necesitaMontura: Boolean, cantKilometros: Double) extends Posta {
    override def puedeParticipar(unParticipante: Participante): Boolean = {
      val reqBasico: Boolean = RequisitoBasicoPosta(unParticipante, this)
      if (necesitaMontura) {
        unParticipante match {
          case Jinete(_, _) => reqBasico
          case _ => false

        }
      } else reqBasico
    }


    override def participar(participantes: List[Participante]): List[Participante] = {
      val participantesHabilitados = super.participar(participantes)
      val mejoresParticipantes = participantesHabilitados.sortWith(CriterioMejorCarrera(_, _))
      return mejoresParticipantes
    }


  }

  object CriterioMejorCarrera {
    def apply(unParticipante: Participante, otroParticipante: Participante): Boolean = {
      val participanteMasRapido: Boolean = unParticipante.velocidad > otroParticipante.velocidad
      participanteMasRapido
    }
  }

  object MejorCorredor {
    def apply(unParticipante: Participante, otroParticipante: Participante): Participante = {
      CriterioMejorCarrera(unParticipante, otroParticipante) match {
        case true => unParticipante
        case false => otroParticipante
      }
    }
  }


  object RequisitoBasicoPosta {
    def apply(unParticipante: Participante, unaPosta: Posta): Boolean = {
      val participanteDelFuturo: Option[Participante] = unParticipante.aumentarHambrePorPosta(unaPosta)
      participanteDelFuturo match {
        case Some(_) => true
        case None => false
      }


    }
  }

    sealed trait Estado{
      def map(f:(Participante => Participante)): Estado
      def flatMap(f: Participante => Estado): Estado
      def filter(f: Participante => Boolean): Estado

    }


    case class Ganador(ganador: Participante) extends Estado{
      override def map(f:(Participante => Participante)): Estado = this
      override def flatMap(f: Participante => Estado): Estado = this

      override def filter(f: (Participante) => Boolean): Estado = this


    }
    case class Participando(participante: Participante) extends Estado{
      override def flatMap(f: (Participante) => Estado): Estado = f(participante)

      override def filter(f: (Participante) => Boolean): Estado = if(f(participante)) this else Perdedor(participante)
      override def map(f: Participante => Participante): Estado = Participando(f(participante))

    }

    case class Perdedor(unPerdedor: Participante) extends Estado{
      override def filter(f: (Participante) => Boolean): Estado = this

      override def flatMap(f: (Participante) => Estado): Estado = this

      override def map(f: (Participante) => Participante): Estado = this

    }


  case class Torneo(vikingos: List[Vikingo],dragones: List[Dragon], postas: List[Posta],regla: Regla){

    def participarEnPosta(vikingos: List[Vikingo], unaPosta: Posta): List[Vikingo] = {
      var dragonesDisponibles: List[Dragon]  = dragones
      val participantes: List[Participante] = for{
          vikingo <- vikingos
          participante <- vikingo.mejorMontura(dragonesDisponibles,unaPosta)
        }yield {
        participante match {
          case Jinete(_,unDragon) => dragonesDisponibles = dragonesDisponibles.filter(dragon => dragon != unDragon); participante
          case Vikingo(_,_,_,_,_,_) => participante
        }
      }
      val resultadoPosta: List[Participante] = unaPosta.participar(participantes)
      val resultadoVikingos: List[Vikingo] = {
        for{
          participante <- resultadoPosta
        }yield {
          participante match{
            case Jinete(unvikingo,undragon) => unvikingo
            case Vikingo(peso,velocidad,barbarosidad,fuerza,nivelhambre,item) => Vikingo(peso,velocidad,barbarosidad,fuerza,nivelhambre,item)
          }
        }
      }
      val clasificados: List[Vikingo] = regla.quienesPasanALaSiguienteRonda(resultadoVikingos)
      clasificados
    }






  }


  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////// R E G L A S ///////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Regla {


    def quienesPasanALaSiguienteRonda(participantes: List[Vikingo]) : List[Vikingo]


  }


  /////////////////////////////////////
  /////// P O R   E Q U I P O S ///////
  /////////////////////////////////////

  case class ReglasPorEquipos() extends Regla{

    override def quienesPasanALaSiguienteRonda(jugadores: List[Vikingo]): List[Vikingo] = {
      jugadores.dropRight(jugadores.length / 2)
    }

    def obtenerGanador(equipos : List[List[Vikingo]]): Option[List[Vikingo]] = Random.shuffle(equipos).headOption

  }

  /////////////////////////////////////
  ///////// E S T A N D A R ///////////
  /////////////////////////////////////

  class ReglasEstandar extends Regla{


    override def quienesPasanALaSiguienteRonda(jugadores: List[Vikingo]): List[Vikingo] = {
      jugadores.take(jugadores.length / 2)
    }

    def obtenerGanador(vikingos: List[Vikingo]): Option[Vikingo] = vikingos.headOption

  }

  /////////////////////////////////////
  /// P O R   E L I M I N A C I O N ///
  /////////////////////////////////////

  case class ReglasDeEliminacion(numeroDeParticipantesEliminadosParaLaSiguienteRonda : Int) extends ReglasEstandar{
    require(numeroDeParticipantesEliminadosParaLaSiguienteRonda>=0)

    override def quienesPasanALaSiguienteRonda(jugadores : List[Vikingo]) : List[Vikingo] = {
      jugadores.reverse.drop(numeroDeParticipantesEliminadosParaLaSiguienteRonda).reverse
    }
  }

  /////////////////////////////////////
  //// T O R N E O   I N V E R S O ////
  /////////////////////////////////////

  case object ReglasDeTorneoInverso extends ReglasEstandar{

    override def quienesPasanALaSiguienteRonda(jugadores: List[Vikingo]): List[Vikingo] = {
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


    override def prepararParaUnaPosta(posta: Posta, vikingos: List[Vikingo], dragones: List[Dragon]): List[Participante] = {
      val dragonesQueCumplenCondicion = dragones.filter(condicionParaLosDragones(_))
      super.prepararParaUnaPosta(posta,vikingos,dragonesQueCumplenCondicion)
    }
  }

  /////////////////////////////////////
  ////// C O N   H A N D I C A P //////
  /////////////////////////////////////

  case object ReglasConHandicap extends ReglasEstandar{


    override def prepararParaUnaPosta(posta: Posta, vikingos: List[Vikingo], dragones: List[Dragon]): List[Participante] = {
      super.prepararParaUnaPosta(posta, vikingos.reverse, dragones)
    }
  }

  type CondicionDragon = Dragon => Boolean




}

