import org.scalatest.FunSuite
import FestivalDelInvierno._

class FestivalDeInvierno extends FunSuite {

  test("Aumentar Hambre Por Posta"){
    var vikingo = Vikingo("Willy",CaracteristicasVikingo(11,2,5,0),None,List())
    var posta=Pesca(Some(10))
    assertResult(vikingo.aumentarHambrePorPosta(posta))(Vikingo("Willy",CaracteristicasVikingo(11,2,5,5),None,List()))
  }

  test("Montar un Dragon"){
    var vikingo = Vikingo("Willy",CaracteristicasVikingo(11,2,5,0),None,List())
    var roco = FuriaNocturna(1,3,15,List(RequisitoBarbarosidad(3)))
    assertResult(vikingo.montar(roco))(Some(Jinete(vikingo,roco)))
  }

  test("Montar un Dragon Con Item"){
    var vikingo = Vikingo("Willy",CaracteristicasVikingo(1,2,5,0),Some(SistemaDeVuelo),List())
    var roco = FuriaNocturna(1,3,15,List(RequisitoBarbarosidad(3),RequisitoItem(SistemaDeVuelo)))
    assertResult(vikingo.montar(roco))(Some(Jinete(vikingo,roco)))
  }

  test("Intenta Montar pero no Puede"){
    var vikingo = Vikingo("Willy",CaracteristicasVikingo(1,2,5,0),Some(SistemaDeVuelo),List())
    var roco = FuriaNocturna(1,3,15,List(RequisitoBarbarosidad(8),RequisitoItem(SistemaDeVuelo)))
    assertResult(vikingo.montar(roco))(None)
  }


  test("Puedo Participar en Posta"){
    var vikingo = Vikingo("Willy",CaracteristicasVikingo(11,2,5,0),None,List())
    var vikingo2 = Vikingo("Willy1",CaracteristicasVikingo(11,2,5,0),None,List())
    var posta=Pesca(Some(10))
    assertResult(posta.participarEnPosta(List(vikingo,vikingo2)))(List(Vikingo("Willy",CaracteristicasVikingo(11,2,5,5),None,List()),Vikingo("Willy1",CaracteristicasVikingo(11,2,5,5),None,List())))
  }

  test("Soy Mejor que Otro"){
    var vikingo = Vikingo("Willy",CaracteristicasVikingo(11,2,5,0),None,List())
    var vikingo2 = Vikingo("Willy1",CaracteristicasVikingo(15,2,5,0),None,List())
    var posta=Pesca(Some(10))

    assert(vikingo2.esMejorQue(vikingo,posta))
  }

  test("Torneo Simple"){
    var vikingo = Vikingo("Willy",CaracteristicasVikingo(11,2,9,0),Some(SistemaDeVuelo),List())
    var vikingo2 = Vikingo("Willy1",CaracteristicasVikingo(15,2,9,0),Some(SistemaDeVuelo),List())
    var posta=Pesca(Some(10))
    var roco = FuriaNocturna(1,3,15,List(RequisitoBarbarosidad(8),RequisitoItem(SistemaDeVuelo)))
    var roco2 = FuriaNocturna(1,3,15,List(RequisitoBarbarosidad(8),RequisitoItem(SistemaDeVuelo)))

    var torneo=Torneo(List(roco,roco2),List(posta))
    var estadoTorneo = RondaEnCurso(List(vikingo,vikingo2))
    assertResult(torneo.jugar(estadoTorneo)(ReglasDeEliminacion(1)))(Some(Vikingo("Willy1",CaracteristicasVikingo(15,2,9,5),Some(SistemaDeVuelo),List())))

  }

  test("Torneo con multiples postas y vikingos"){
    var vikingo = Vikingo("Willy",CaracteristicasVikingo(15, 2, 5, 0), None, List())
    var vikingo2 = Vikingo("Willy2",CaracteristicasVikingo(12, 2, 6, 0), None, List())
    var vikingo3 = Vikingo("Willy3",CaracteristicasVikingo(13, 2, 7, 0), None, List())
    var vikingo4 = Vikingo("Willy4",CaracteristicasVikingo(14, 2, 8, 0), None, List())
    var posta=Pesca(Some(10))
    var posta2=Combate(1)
    var posta3=Carrera(15,true)
    var roco = FuriaNocturna(1,3,15,List(RequisitoBarbarosidad(1)))
    var roco2 = FuriaNocturna(1,3,15,List(RequisitoBarbarosidad(1)))
    var torneo=Torneo(List(roco,roco2),List(posta,posta2,posta3))
    var estadoTorneo = RondaEnCurso(List(vikingo,vikingo2,vikingo3,vikingo4))
    assertResult(torneo.jugar(estadoTorneo)(ReglasDeEliminacion(1)))(Some(Vikingo("Willy3",CaracteristicasVikingo(13,2,7,15),None,List())))


  }

  test("Torneo se juegan todas las postas"){
    var vikingo = Vikingo("Willy",CaracteristicasVikingo(15, 2, 5, 0), None, List())
    var vikingo2 = Vikingo("Willy2",CaracteristicasVikingo(12, 2, 6, 0), None, List())
    var vikingo3 = Vikingo("Willy3",CaracteristicasVikingo(13, 2, 7, 0), None, List())
    var vikingo4 = Vikingo("Willy4",CaracteristicasVikingo(14, 2, 8, 0), None, List())
    var posta2=Combate(2)
    var posta=Carrera(15,false)
    var posta3=Pesca(None)
    var roco = FuriaNocturna(1,3,15,List(RequisitoBarbarosidad(1)))
    var roco2 = FuriaNocturna(1,3,15,List(RequisitoBarbarosidad(1)))
    var torneo=Torneo(List(roco,roco2),List(posta,posta3))
    assertResult(torneo.jugar(RondaEnCurso(List(vikingo,vikingo2,vikingo3,vikingo4)))(ReglasDeEliminacion(1)))(Some(Vikingo("Willy3",CaracteristicasVikingo(13,2,7,20),None,List())))


  }

  test("Torneo Por Equipos Que Obtiene Ganador Antes De Jugar Todas Las Postas") {
    var vikingo = Vikingo("Willy",CaracteristicasVikingo(15, 2, 5, 0), None, List())
    var vikingo2 = Vikingo("Willy2",CaracteristicasVikingo(12, 3, 6, 0), None, List())
    var vikingo3 = Vikingo("Willy3",CaracteristicasVikingo(13, 19, 7, 0), None, List())
    var vikingo4 = Vikingo("Willy4",CaracteristicasVikingo(14, 20, 8, 0), None, List())
    var equipoVikingo = List(vikingo,vikingo2)
    var equipoVikingo1 = List(vikingo3,vikingo4)
    var posta = Carrera(15, false)
    var posta3 = Pesca(Some(10))
    var roco = FuriaNocturna(1, 3, 15, List(RequisitoBarbarosidad(1)))
    var roco2 = FuriaNocturna(1, 3, 15, List(RequisitoBarbarosidad(1)))
    var torneo = Torneo(List(roco, roco2), List(posta,posta3))
    var estadoTorneo = RondaPorEquipos(List(equipoVikingo,equipoVikingo1))
    var equipoGanador = EquipoVikingo(List(Vikingo("Willy4",CaracteristicasVikingo(14.0,20.0,8.0,15.0),None,List()), Vikingo("Willy3",CaracteristicasVikingo(13.0,19.0,7.0,15.0),None,List())))
    assertResult(torneo.jugar(estadoTorneo)(ReglasPorEquipos()))(Some(equipoGanador))

  }


  test("Torneo Por Equipos Que Obtiene Ganador Despues De Jugar Todas Las Postas") {
    var vikingo = Vikingo("Willy",CaracteristicasVikingo(15, 2, 5, 0), None, List())
    var vikingo2 = Vikingo("Willy2",CaracteristicasVikingo(12, 19, 6, 0), None, List())
    var vikingo3 = Vikingo("Willy3",CaracteristicasVikingo(13, 9, 7, 0), None, List())
    var vikingo4 = Vikingo("Willy4",CaracteristicasVikingo(14, 20, 8, 0), None, List())
    var equipoVikingo = List(vikingo,vikingo2)
    var equipoVikingo1 = List(vikingo3,vikingo4)
    var posta = Carrera(15, false)
    var posta3 = Pesca(Some(10))
    var roco = FuriaNocturna(1, 3, 15, List(RequisitoBarbarosidad(1)))
    var roco2 = FuriaNocturna(1, 3, 15, List(RequisitoBarbarosidad(1)))
    var torneo = Torneo(List(roco, roco2), List(posta, posta3))
    var estadoTorneo = RondaPorEquipos(List(equipoVikingo,equipoVikingo1))
    var equipoGanador = EquipoVikingo(List(Vikingo("Willy4",CaracteristicasVikingo(14.0,20.0,8.0,20.0),None,List())))
    assertResult(torneo.jugar(estadoTorneo)(ReglasPorEquipos()))(Some(equipoGanador))

  }


  test("Vikingos poco deportivos"){
    def hambreMenorA(vikingo: Vikingo) : Boolean= vikingo.nivelHambre<50
    var patapez=Vikingo("Willy",CaracteristicasVikingo(12,3,4,0),Some(Comestible(40)), List(hambreMenorA))
    var posta=Pesca(Some(10))
    assertResult(patapez.participarEnMiMejorFormaEnUnaPosta(posta,List()))(Some(patapez))
  }

  test("Vikingos poco deportivos no pueden competir"){
    def hambreMenorA(vikingo: Vikingo) : Boolean = vikingo.nivelHambre<50
    var patapez = Vikingo("Willy",CaracteristicasVikingo(12,3,4,60),Some(Comestible(40)), List(hambreMenorA))
    var posta = Pesca(Some(10))
    assert(!(patapez.puedoCompetir))
  }

  test("Vikingos poco deportivos pueden competir"){
    def hambreMenorA(vikingo: Vikingo) : Boolean= vikingo.nivelHambre<50
    var patapez = Vikingo("Willy",CaracteristicasVikingo(12,3,4,20),Some(Comestible(40)), List(hambreMenorA))
    var posta = Pesca(Some(10))
    assert(patapez.puedoCompetir)
  }

  test("Vikingos poco deportivos comen despues de cada posta"){
    def hambreMenorA(vikingo: Vikingo) : Boolean= vikingo.nivelHambre<50
    var patapez=Vikingo("Willy",CaracteristicasVikingo(40,3,30,20),Some(Comestible(40)), List(hambreMenorA))
    var posta=Pesca(Some(10))
    var vikingo2 = Vikingo("Willy2",CaracteristicasVikingo(15,2,9,0),Some(SistemaDeVuelo), List())
    var torneo=Torneo(List(),List(posta))
    var estadoTorneo = RondaEnCurso(List(patapez,vikingo2))
    assertResult(torneo.jugar(estadoTorneo)(ReglasDeEliminacion(1)))(Some(patapez.modificarNivelHambre(0)))

  }

}
