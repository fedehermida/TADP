
/**
  * Created by TYPE Null on 9/6/2017.
  */

package object FestivalDelInvierno {
  import scala.util.Failure
  import scala.util.Try
  import scala.util.Success


  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////// P A R T I C I P A N T E S //////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  trait Participante {
    def maximoKilosPescado: Double

    def barbarosidad: Double

    def tieneArma: Boolean

    def danio: Double

    def velocidad: Double

    def nivelDeHambre: Double

    def aumentarHambrePorPosta(unaPosta: Posta): Try[Participante]

    def esMejorQue(unParticipante: Participante, unaPosta: Posta): Boolean = {
      unaPosta match {
        case Pesca(_) => CriterioMejorPescador(this, unParticipante)
        case Combate(_) => CriterioMejorCombate(this, unParticipante)
        case Carrera(_, _) => CriterioMejorCarrera(this, unParticipante)
      }

      def mejorEnPosta(unParticipante: Participante, unaPosta: Posta): Participante = {
        unaPosta match {
          case Pesca(_) => MejorPescador(this, unParticipante)
          case Combate(_) => MejorCombatiente(this, unParticipante)
          case Carrera(_, _) => MejorCorredor(this, unParticipante)
        }

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

    def aumentarHambre(unaCantidad: Double): Try[Vikingo] = {
      val hambriento: Boolean = nivelDeHambre + unaCantidad < 100
      hambriento match {
        case false => Success(this.copy(nivelDeHambre = nivelDeHambre + unaCantidad))
        case true => Failure(throw new IllegalArgumentException)
      }
    }

    override def aumentarHambrePorPosta(unaPosta: Posta): Try[Vikingo] = {
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
        case Some(Arma(unDanio)) => true
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

      val mejoresCombinaciones: List[Participante] =  for  {
        dragon <- dragones if dragon.puedeMontarme(this)
        unJinete: Jinete = montar(dragon)
        unaPosta.puedeParticipar(unJinete)

      }yield if (this.esMejorQue(unJinete,unaPosta) && unaPosta.puedeParticipar(this)) this
              else unJinete

      val mejorForma: Option[Participante] = mejoresCombinaciones.sortWith( (participante,otro) => participante.esMejorQue(otro,unaPosta)).headOption
      mejorForma
    }
  }

  trait Item

  case class Arma(danio: Double) extends Item{
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
      unVikingo.item == unObjeto
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
    override def maximoKilosPescado = unVikingo.peso - unDragon.peso * 0.2
    override def barbarosidad: Double = unVikingo.barbarosidad
    override def danio = unVikingo.fuerza + unDragon.danio
    override def velocidad = unDragon.velocidad - unVikingo.peso
    override def tieneArma: Boolean = unVikingo.tieneArma
    override def nivelDeHambre: Double = unVikingo.nivelDeHambre
    override def aumentarHambrePorPosta(unaPosta: Posta): Try[Participante] = unVikingo.aumentarHambre(5)
  }

  trait Posta {
    def puedeParticipar(unParticipante: Participante): Boolean
    def participar(participantes: List[Participante]): List[Participante]
  }

  case class Pesca(pesoMinimo: Option[Double]) extends Posta {
    override def puedeParticipar(unParticipante: Participante): Boolean = {
      val reqBasico: Boolean = RequisitoBasicoPosta(unParticipante,this)
      this.pesoMinimo match {
        case Some(num) => (unParticipante.maximoKilosPescado > num) && reqBasico
        case _ => true && reqBasico
      }
    }

    override def participar(participantes: List[Participante]): List[Participante] = {
      val puedenParticipar = participantes.filter(this.puedeParticipar(_))
      val mejoresParticipantes = puedenParticipar.sortWith(CriterioMejorPescador(_,_))
      val participanteModificado = mejoresParticipantes.map(_.aumentarHambrePorPosta(this).get)
      return participanteModificado
    }

  }

  object CriterioMejorPescador{
    def apply(unParticipante: Participante, otroParticipante: Participante): Boolean = unParticipante.maximoKilosPescado > otroParticipante.maximoKilosPescado
    }

   object MejorPescador{
     def apply(unParticipante: Participante, otroParticipante: Participante): Participante = {
       if(CriterioMejorPescador(unParticipante,otroParticipante)) unParticipante
       else otroParticipante
     }
   }

  case class Combate(barbarosidadMinima: Double) extends Posta{
    override def puedeParticipar(unParticipante: Participante): Boolean = {
      val reqBasico: Boolean = RequisitoBasicoPosta(unParticipante,this)
      val superaBarbarosidadMinima = unParticipante.barbarosidad > barbarosidadMinima
      val tieneArma = unParticipante.tieneArma
      val cumpleCondicion = (tieneArma | superaBarbarosidadMinima) && reqBasico
      cumpleCondicion
    }

    override def participar(participantes: List[Participante]): List[Participante] = {
      val puedenParticipar = participantes.filter(this.puedeParticipar(_))
      val mejoresParticipantes = puedenParticipar.sortWith(CriterioMejorCombate(_,_))
      val participanteModificado = mejoresParticipantes.map(_.aumentarHambrePorPosta(this).get)
      return participanteModificado
    }

  }

  object CriterioMejorCombate{
    def apply(unParticipante: Participante, otroParticipante: Participante): Boolean = {
      val mejorParticipante = unParticipante.danio > otroParticipante.danio
      mejorParticipante
    }
  }

  object MejorCombatiente{
      def apply(unParticipante: Participante, otroParticipante: Participante):Participante = {

        CriterioMejorCombate(unParticipante,otroParticipante) match{
        case true => unParticipante
        case _ => otroParticipante
      }
    }
  }

  case class Carrera(necesitaMontura: Boolean,cantKilometros: Double)extends Posta{
    override def puedeParticipar(unParticipante: Participante): Boolean = {
      val reqBasico: Boolean = RequisitoBasicoPosta(unParticipante,this)
      if(necesitaMontura){
        unParticipante match{
          case Jinete(_,_) => true && reqBasico
          case _ => false

        }
      }else (true && reqBasico)
    }


    override def participar(participantes: List[Participante]): List[Participante] = {
      val puedenParticipar = participantes.filter(this.puedeParticipar(_))
      val mejoresParticipantes = puedenParticipar.sortWith(CriterioMejorCarrera(_,_))
      val participanteModificado = mejoresParticipantes.map(_.aumentarHambrePorPosta(this).get)
      return participanteModificado
    }


 }

  object CriterioMejorCarrera{
    def apply(unParticipante: Participante, otroParticipante: Participante): Boolean = {
      val participanteMasRapido: Boolean = unParticipante.velocidad > otroParticipante.velocidad
      participanteMasRapido
    }
  }
  object MejorCorredor{
    def apply(unParticipante: Participante, otroParticipante: Participante): Participante ={
      CriterioMejorCarrera(unParticipante,otroParticipante) match{
        case true => unParticipante
        case false => otroParticipante
      }
    }
  }


  object RequisitoBasicoPosta{
    def apply(unParticipante: Participante, unaPosta: Posta): Boolean = {
      val participanteDelFuturo: Try[Participante] = unParticipante.aumentarHambrePorPosta(unaPosta)
      participanteDelFuturo match{
        case Success(_) => true
        case Failure(_) => false
      }


    }
  }

  case class Torneo(postas: List[Posta], vikingos: List[Vikingo], dragones: Set[Dragon]) {



  }
}