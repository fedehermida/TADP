package main

/**
  * Created by leonardo on 03/06/17.
  */
package object FestivalDeInvierno {

  class Jinete(unVikingo:Vikingo,unDragon:Dragon){

  }


  case class Vikingo(item:Item,
                    caracteristicas: Caracteristicas){

    def peso = this.caracteristicas.peso
    def velocidad=this.caracteristicas.velocidad
    def hambre=this.caracteristicas.hambre
    def barbarosidad=this.caracteristicas.barbarosidad

    def calcularDanio():Int=5



    def puedoMontarUnaRaza(unDragon:Dragon):Boolean={
        unDragon.asInstanceOf match{
          case _:NadderMortifero => unDragon.danio > this.calcularDanio()
          case _:Gronckle=>Gronckle.pesoMaximo > this.peso

        }
    }


  }
  }

  case class Caracteristicas(peso:Int,
                             velocidad:Int,
                             barbarosidad:Int,
                             hambre:Int){

  }

  case class Item(danio:Int){

  }

  abstract class Dragon(){



  }


  case class NadderMortifero(velocidadBase:Int,peso:Int, danio:Int) extends Dragon{


  }

  case class Gronckle(velocidadBase:Int,peso:Int,danio:Int,pesoMaximo:Int) extends Dragon{



  }

