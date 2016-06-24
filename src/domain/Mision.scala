package domain

import scala.util.Success
import scala.util.Try
import scala.util.Failure
import scala.util.control.Exception

trait ResultadoMision {
	def equipo: Equipo
	def map(f: (Equipo => Equipo)): ResultadoMision
	def filter(f: (Equipo => Boolean)): ResultadoMision
	def flatMap(f: (Equipo => ResultadoMision)): ResultadoMision
	def fold[T](e: (Equipo => T))(f: (Equipo => T)): T
}

case class Exitosa(val equipo: Equipo,tarea:Tarea) extends ResultadoMision {
	def map(f: (Equipo => Equipo)) = Exitosa(f(equipo),tarea)
			def filter(f: (Equipo => Boolean)) = if (f(equipo)) this else Fallida(equipo,tarea,"El equipo no cumplio la condicion")
			def flatMap(f: (Equipo => ResultadoMision)) = f(equipo)
			def fold[T](e: (Equipo => T))(f: (Equipo => T)): T = f(equipo)
}

case class Fallida(val equipo: Equipo,tarea:Tarea, descripcion: String) extends ResultadoMision {
	def map(f: (Equipo => Equipo)) = this
			def filter(f: (Equipo => Boolean)) = this
			def flatMap(f: (Equipo => ResultadoMision)) = this
			def fold[T](e: (Equipo => T))(f: (Equipo => T)): T = e(equipo)
}

case class Mision(tareas:List[Tarea], recompensa: (Equipo => Equipo)) {

	def serRealizadaPor(equipo:Equipo):ResultadoMision =  {
	  val estadoInicial:ResultadoMision = Exitosa(equipo,null)
		val resultado = tareas.foldLeft(estadoInicial)((e,tarea) => tarea.realizar(e))
		resultado.map { e => this.recompensa(e) }
	}

}


case class Tarea(facilidad:((Heroe, Equipo) => Int), condicion:(Equipo => Boolean), efectoSobre:((Heroe,Equipo)=> Equipo)) {
  
  def realizar(equipoEnMision:ResultadoMision):ResultadoMision = {
      equipoEnMision.filter { equipo => this.condicion(equipo)}
                    .flatMap { e => equipoConMejorHeroe(e) }
  }
  
  def equipoConMejorHeroe(equipo:Equipo):ResultadoMision = {
    this.mejorHeroeParaTarea(equipo) match{
      case Some(x) => Exitosa(efectoSobre(x,equipo),this)
      case _ => Fallida(equipo,this,"Nadie puede realizarla")
    }
  }
  
  def mejorHeroeParaTarea(equipo:Equipo):Option[Heroe] = {
    equipo.mejorHeroeSegun { heroe => this.facilidad(heroe,equipo) } 
  }

  def verificarCondicion(equipo:Option[Equipo]):Option[Equipo] = {
    equipo.filter { equipo => this.condicion(equipo)}
  }
    
  //--------para test---------
  def serRealizadaPorTest(equipo:Equipo) = {   
    realizar(Exitosa(equipo,this))match{
      case Exitosa(x,t) => x
      case _ => equipo
    }
  }
  
}
