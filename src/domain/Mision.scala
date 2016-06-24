package domain

import scala.util.Success
import scala.util.Try
import scala.util.Failure
import scala.util.control.Exception

  trait ResultadoDeEjecucion {
    def equipo: Equipo
    def map(f: (Equipo => Equipo)): ResultadoDeEjecucion
    def filter(f: (Equipo => Boolean)): ResultadoDeEjecucion
    def flatMap(f: (Equipo => ResultadoDeEjecucion)): ResultadoDeEjecucion
    def fold[T](e: (Equipo => T))(f: (Equipo => T)): T
  }

  case class Ejecutando(val equipo: Equipo,tarea:Tarea) extends ResultadoDeEjecucion {
    def map(f: (Equipo => Equipo)) = Ejecutando(f(equipo),tarea)
    def filter(f: (Equipo => Boolean)) = if (f(equipo)) this else Fallida(equipo,tarea,"Fallo el filtrado")
    def flatMap(f: (Equipo => ResultadoDeEjecucion)) = f(equipo)
    def fold[T](e: (Equipo => T))(f: (Equipo => T)): T = f(equipo)
  }

  case class Fallida(val equipo: Equipo,tarea:Tarea, descripcion: String) extends ResultadoDeEjecucion {
    def map(f: (Equipo => Equipo)) = this
    def filter(f: (Equipo => Boolean)) = this
    def flatMap(f: (Equipo => ResultadoDeEjecucion)) = this
    def fold[T](e: (Equipo => T))(f: (Equipo => T)): T = e(equipo)
  }

class Mision(tareas:List[Tarea], recompensa: (Equipo => Equipo)) {
     
   def realizarMision(equipo:Equipo):ResultadoDeEjecucion =  {
     var team: ResultadoDeEjecucion= Ejecutando(equipo,tareas.head) 
     team = tareas.foldLeft(team)((e,tarea) => tarea.realizar(e))
     team.map { e => this.recompensa(e) }
     }
   
   //tambien se puede hacer q el resultado devuelva equipo y tarea
   def equipoAlrealizar(equipo:Equipo):Equipo = {
     realizarMision(equipo) match{
           case Ejecutando(e,t) => this.recompensa(e)
           case Fallida(e,t,d) => equipo  //para test
        }
   }
   
}


case class Tarea(facilidad:((Heroe, Equipo) => Int), condicion:(Equipo => Boolean), efectoSobre:((Heroe,Equipo)=> Equipo)) {
  
  def realizar(equipo:ResultadoDeEjecucion):ResultadoDeEjecucion = {
      var team =     equipo.filter { equipo => this.condicion(equipo)}
      team.flatMap { e => equipoConMejorHeroe(e) }
  }
  
  def mejorHeroeParaTarea(equipo:Equipo):Option[Heroe] = {
    equipo.mejorHeroeSegun { heroe => this.facilidad(heroe,equipo) } 
  }
  
  def equipoConMejorHeroe(equipo:Equipo):ResultadoDeEjecucion = {
    this.mejorHeroeParaTarea(equipo) match{
      case Some(x) => Ejecutando(efectoSobre(x,equipo),this)
      case _ => Fallida(equipo,this,"Nadie puede realizarla")
    }
  }

  def verificarCondicion(equipo:Option[Equipo]):Option[Equipo] = {
    equipo.filter { equipo => this.condicion(equipo)}
  }
    
  //--------para test---------
  def serRealizadaPorTest(equipo:Equipo) = {   
    realizar(Ejecutando(equipo,this))match{
      case Ejecutando(x,t) => x
      case _ => equipo
    }
  }
  
}
