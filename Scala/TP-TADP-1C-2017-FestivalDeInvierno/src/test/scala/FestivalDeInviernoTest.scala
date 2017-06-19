/**
  * Created by leonardo on 19/06/17.
  */
import org.scalatest.FunSuite
import FestivalDelInvierno._

class FestivalDeInviernoTest extends FunSuite {

 test("Aumentar Hambre Por Posta"){
   var vikingo = Vikingo(CaracteristicasVikingo(11,2,5,0),None)
   var posta=Pesca(Some(10))
   assertResult(vikingo.aumentarHambrePorPosta(posta))(Vikingo(CaracteristicasVikingo(11,2,5,5),None))
 }

  test("Montar un Dragon"){
    var vikingo = Vikingo(CaracteristicasVikingo(1,2,5,0),None)
    var roco = FuriaNocturna(1,3,15,10,0,List(RequisitoBarbarosidad(3)))
    assertResult(vikingo.montar(roco))(Some(Jinete(vikingo,roco)))
  }

  test("Montar un Dragon Con Item"){
    var vikingo = Vikingo(CaracteristicasVikingo(1,2,5,0),Some(SistemaDeVuelo))
    var roco = FuriaNocturna(1,3,15,10,0,List(RequisitoBarbarosidad(3),RequisitoItem(Some(SistemaDeVuelo))))
    assertResult(vikingo.montar(roco))(Some(Jinete(vikingo,roco)))
  }

  test("Intenta Montar pero no Puede"){
    var vikingo = Vikingo(CaracteristicasVikingo(1,2,5,0),Some(SistemaDeVuelo))
    var roco = FuriaNocturna(1,3,15,10,0,List(RequisitoBarbarosidad(8),RequisitoItem(Some(SistemaDeVuelo))))
    assertResult(vikingo.montar(roco))(None)
  }


  test("Puedo Participar en Posta"){
    var vikingo = Vikingo(CaracteristicasVikingo(11,2,5,0),None)
    var vikingo2 = Vikingo(CaracteristicasVikingo(11,2,5,0),None)
    var posta=Pesca(Some(10))
    assertResult(posta.participarEnPosta(List(vikingo,vikingo2)))(List(Vikingo(CaracteristicasVikingo(11,2,5,5),None),Vikingo(CaracteristicasVikingo(11,2,5,5),None)))
  }

  test("Soy Mejor que Otro"){
    var vikingo = Vikingo(CaracteristicasVikingo(11,2,5,0),None)
    var vikingo2 = Vikingo(CaracteristicasVikingo(15,2,5,0),None)
    var posta=Pesca(Some(10))

    assert(vikingo2.esMejorQue(vikingo,posta))
  }



}
