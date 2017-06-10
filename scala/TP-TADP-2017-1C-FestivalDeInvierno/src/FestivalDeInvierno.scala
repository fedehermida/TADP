/**
  * Created by Cabeza de Tacho on 9/6/2017.
  */
object FestivalDelInvierno {

  abstract class Participante {

    def cantidadDeCarga: Double

    def barbarosidad: Int

    def tieneArma: Boolean
/*
    def puedoPescar(condicionPostas: CondicionPostas):Boolean={
      condicionPostas.puedeParticipar(this)
    }
*/
  }

  case class Vikingo(caracteristica: CaracteristicasVikingo, item: Option[Item]) extends Participante {

    def peso = caracteristica.peso

    override def barbarosidad = caracteristica.barbarosidad

    def velocidad = caracteristica.velocidad

    def nivelHambre = caracteristica.nivelHambre

    def danio = barbarosidad

    override def tieneArma: Boolean = item match {

      case Some(Arma(_)) => true
      case _ => false

    }

    def modificarNivelHambre(nuevoHambre: Double): Unit = copy(caracteristica = caracteristica.copy(nivelHambre = nuevoHambre))

    def montar(unDragon: Dragon): Option[Jinete] = {
      if (unDragon.condicionesDeMonturaDragon.forall(condicion => condicion.cumpleRequisito(this, unDragon)))
        Some(Jinete(this, unDragon))
      else
        None
    }

    override def cantidadDeCarga: Double = peso / 2 + barbarosidad * 2

  }

  case class CaracteristicasVikingo(peso: Int, velocidad: Int, barbarosidad: Int, nivelHambre: Double)

  abstract class Item

  case class Arma(unDanio: Int) extends Item

  case object SistemaDeVuelo extends Item

  case class Comestible(disminuirHambre: Double) extends Item


  case class Dragon(raza: Raza,
                    velocidadBase: Double,
                    peso: Double,
                    velocidadDeVuelo: Double,
                    danio: Double,
                    cargaMaxima: Double,
                    cargaActual: Double,
                    condicionesDeMonturaDragon: List[CondicionMontura]) {}


  object Dragon {
    def apply(razaDragon: Raza, pesoDragon: Double, velocidadBaseDragon: Double = 60, danioDragon: Double, condicionesDeMonturaDragon: List[CondicionMontura]): Dragon = {
      razaDragon match {
        case FuriaNocturna => Dragon(raza = razaDragon,
          velocidadBase = velocidadBaseDragon,
          peso = pesoDragon,
          velocidadDeVuelo = velocidadBaseDragon - pesoDragon,
          danio = danioDragon,
          cargaMaxima = pesoDragon * 0.2,
          cargaActual = 0,
          condicionesDeMonturaDragon = condicionesDeMonturaDragon)

        case Gronckle => Dragon(raza = razaDragon,
          velocidadBase = velocidadBaseDragon / 2,
          peso = pesoDragon,
          velocidadDeVuelo = velocidadBaseDragon - pesoDragon,
          danio = pesoDragon * 5,
          cargaMaxima = pesoDragon * 0.2,
          cargaActual = 0,
          condicionesDeMonturaDragon = condicionesDeMonturaDragon)

        case NadderMortifero => Dragon(raza = razaDragon,
          velocidadBase = velocidadBaseDragon,
          peso = pesoDragon,
          velocidadDeVuelo = velocidadBaseDragon - pesoDragon,
          danio = 150,
          cargaMaxima = pesoDragon * 0.2,
          cargaActual = 0,
          condicionesDeMonturaDragon = condicionesDeMonturaDragon)

      }
    }
  }

  sealed trait Raza

  case object NadderMortifero extends Raza

  case object Gronckle extends Raza

  case object FuriaNocturna extends Raza


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
      unVikingo.peso <= pesoMaximo
    }
  }

  case class Jinete(unVikingo: Vikingo, unDragon: Dragon) extends Participante {

    override def cantidadDeCarga: Double = unDragon.peso / 5 + unDragon.danio

    def danio: Double = unVikingo.danio + unDragon.danio

    def velocidad: Double = unDragon.velocidadDeVuelo - unVikingo.peso

    override def barbarosidad: Int = unVikingo.barbarosidad

    override def tieneArma: Boolean = unVikingo.item match {

      case Some(Arma(_)) => true
      case _ => false

    }

  }



  abstract class CondicionPostas{
    def puedeParticipar:Boolean
  }


  case class CondicionPesca(pesoMinimo: Int, participante: Participante) extends CondicionPostas {

    override def puedeParticipar: Boolean = participante.cantidadDeCarga > pesoMinimo
  }

  case class CondicionCombate(unParticipante: Participante, barbaridadMinima: Int) extends CondicionPostas{
    override def puedeParticipar: Boolean = {

      unParticipante.tieneArma || unParticipante.barbarosidad < barbaridadMinima


    }
  }
  case class CondicionCarrera(requiereMontura: Boolean,unParticipante: Participante) extends CondicionPostas{
    override def puedeParticipar: Boolean = {
      if (requiereMontura)
        unParticipante match {
          case Jinete(_, _) => true
          case _ => false
        }
      true
    }
  }

  type Posta = List[Participante] => CondicionPostas => List[Participante]

  object postas {

    def pesca(participantes: List[Participante], condicionPosta: CondicionPesca): List[Participante] = ???

  }


    /*
    def condicionGeneral(unParticipante: Participante): Boolean = {

    unParticipante match {

      case Jinete(unVikingo,_) => unVikingo.nivelHambre + 5 < 100
      case Vikingo(caracteristica,_) => caracteristica.nivelHambre + 5 < 100 //Desarrollar metodo

    }

    }
  }
*/

  }


