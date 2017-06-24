/**
  * Created by leonardo on 19/06/17.
  */
import org.scalatest.FunSuite
import FestivalDelInvierno._

class FestivalDeInviernoTest extends FunSuite {

  test("Aumentar Hambre Por Posta"){
    var vikingo = Vikingo(CaracteristicasVikingo(11,2,5,0),None,None)
    var posta=Pesca(Some(10))
    assertResult(vikingo.aumentarHambrePorPosta(posta))(Vikingo(CaracteristicasVikingo(11,2,5,5),None,None))
  }

  test("Montar un Dragon"){
    var vikingo = Vikingo(CaracteristicasVikingo(11,2,5,0),None,None)
    var roco = FuriaNocturna(1,3,15,List(RequisitoBarbarosidad(3)))
    assertResult(vikingo.montar(roco))(Some(Jinete(vikingo,roco)))
  }

  test("Montar un Dragon Con Item"){
    var vikingo = Vikingo(CaracteristicasVikingo(1,2,5,0),Some(SistemaDeVuelo),None)
    var roco = FuriaNocturna(1,3,15,List(RequisitoBarbarosidad(3),RequisitoItem(SistemaDeVuelo)))
    assertResult(vikingo.montar(roco))(Some(Jinete(vikingo,roco)))
  }

  test("Intenta Montar pero no Puede"){
    var vikingo = Vikingo(CaracteristicasVikingo(1,2,5,0),Some(SistemaDeVuelo),None)
    var roco = FuriaNocturna(1,3,15,List(RequisitoBarbarosidad(8),RequisitoItem(SistemaDeVuelo)))
    assertResult(vikingo.montar(roco))(None)
  }


  test("Puedo Participar en Posta"){
    var vikingo = Vikingo(CaracteristicasVikingo(11,2,5,0),None,None)
    var vikingo2 = Vikingo(CaracteristicasVikingo(11,2,5,0),None,None)
    var posta=Pesca(Some(10))
    assertResult(posta.participarEnPosta(List(vikingo,vikingo2)))(List(Vikingo(CaracteristicasVikingo(11,2,5,5),None,None),Vikingo(CaracteristicasVikingo(11,2,5,5),None,None)))
  }

  test("Soy Mejor que Otro"){
    var vikingo = Vikingo(CaracteristicasVikingo(11,2,5,0),None,None)
    var vikingo2 = Vikingo(CaracteristicasVikingo(15,2,5,0),None,None)
    var posta=Pesca(Some(10))

    assert(vikingo2.esMejorQue(vikingo,posta))
  }

  test("Torneo Simple"){
    var vikingo = Vikingo(CaracteristicasVikingo(11,2,9,0),Some(SistemaDeVuelo),None)
    var vikingo2 = Vikingo(CaracteristicasVikingo(15,2,9,0),Some(SistemaDeVuelo),None)
    var posta=Pesca(Some(10))
    var roco = FuriaNocturna(1,3,15,List(RequisitoBarbarosidad(8),RequisitoItem(SistemaDeVuelo)))
    var roco2 = FuriaNocturna(1,3,15,List(RequisitoBarbarosidad(8),RequisitoItem(SistemaDeVuelo)))

    var torneo=Torneo(Vikingos(List(vikingo,vikingo2)),List(roco,roco2),List(posta),ReglasDeEliminacion(1))

    assertResult(torneo.jugar())(Some(Vikingo(CaracteristicasVikingo(15,2,9,5),Some(SistemaDeVuelo),None)))

  }

  test("Torneo con multiples postas y vikingos"){
    var vikingo = Vikingo(CaracteristicasVikingo(11,2,5,0),None,None)
    var vikingo2 = Vikingo(CaracteristicasVikingo(12,2,5,0),None,None)
    var vikingo3 = Vikingo(CaracteristicasVikingo(13,2,5,0),None,None)
    var vikingo4 = Vikingo(CaracteristicasVikingo(14,2,5,0),None,None)
    var posta=Pesca(Some(10))
    var posta2=Combate(1)
    var posta3=Carrera(15,true)
    var roco = FuriaNocturna(1,3,15,List(RequisitoBarbarosidad(1)))
    var roco2 = FuriaNocturna(1,3,15,List(RequisitoBarbarosidad(1)))
    var torneo=Torneo(Vikingos(List(vikingo,vikingo2,vikingo3,vikingo4)),List(roco,roco2),List(posta,posta2,posta3),ReglasDeEliminacion(1))
    assertResult(torneo.jugar())(Some(Vikingo(CaracteristicasVikingo(13,2,5,15),None,None)))


  }

  test("Torneo se juegan todas las postas"){
    var vikingo = Vikingo(CaracteristicasVikingo(15,2,5,0),None,None)
    var vikingo2 = Vikingo(CaracteristicasVikingo(12,2,6,0),None,None)
    var vikingo3 = Vikingo(CaracteristicasVikingo(13,2,7,0),None,None)
    var vikingo4 = Vikingo(CaracteristicasVikingo(14,2,8,0),None,None)
    var posta2=Combate(2)
    var posta=Carrera(15,false)
    var posta3=Pesca(Some(10))
    var roco = FuriaNocturna(1,3,15,List(RequisitoBarbarosidad(1)))
    var roco2 = FuriaNocturna(1,3,15,List(RequisitoBarbarosidad(1)))
    var torneo=Torneo(Vikingos(List(vikingo,vikingo2,vikingo3,vikingo4)),List(roco,roco2),List(posta,posta3),ReglasDeEliminacion(1))
    assertResult(torneo.jugar())(Some(Vikingo(CaracteristicasVikingo(13,2,7,20),None,None)))


  }

  test("Torneo Por Equipos") {
    var vikingo = Vikingo(CaracteristicasVikingo(15, 2, 5, 0), None, None)
    var vikingo2 = Vikingo(CaracteristicasVikingo(12, 2, 6, 0), None, None)
    var vikingo3 = Vikingo(CaracteristicasVikingo(13, 2, 7, 0), None, None)
    var vikingo4 = Vikingo(CaracteristicasVikingo(14, 2, 8, 0), None, None)
    var equipoVikingo=new EquipoVikingo
    var equipoVikingo1=new EquipoVikingo
    equipoVikingo.agregarVikingos(List(vikingo,vikingo2))
    equipoVikingo1.agregarVikingos(List(vikingo3,vikingo4))
    var posta = Carrera(15, false)
    var posta3 = Pesca(Some(10))
    var roco = FuriaNocturna(1, 3, 15, List(RequisitoBarbarosidad(1)))
    var roco2 = FuriaNocturna(1, 3, 15, List(RequisitoBarbarosidad(1)))
    var torneo = Torneo(EquiposVikingos(List(equipoVikingo,equipoVikingo1)), List(roco, roco2), List(posta, posta3), ReglasPorEquipos())
    assertResult(torneo.jugar())(Some(equipoVikingo1))

  }


}

