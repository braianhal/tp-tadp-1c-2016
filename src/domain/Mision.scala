package domain

import scala.util.Success
import scala.util.Try
import scala.util.Failure
import scala.util.control.Exception

trait ResultadoMision {
	def equipo: Equipo
	def map(f: (Equipo => Equipo)): ResultadoMision
	def map(f: ((Tarea,Equipo) => (Tarea,Equipo))): ResultadoMision
	def filter(f: ((Equipo,Tarea) => Boolean)): ResultadoMision
	def flatMap(f: (Equipo => ResultadoMision)): ResultadoMision
	def fold[T](e: (Equipo => T))(f: (Equipo => T)): T
}

case class Exitosa(val equipo: Equipo,tarea:Tarea) extends ResultadoMision {
	def map(f: (Equipo => Equipo)) = Exitosa(f(equipo),tarea)
	def map(f: ((Tarea,Equipo) => (Tarea,Equipo))) = {
	  val (tareaMapeada,equipoMapeado) = f(tarea,equipo)
	  Exitosa(equipoMapeado,tareaMapeada)
	}
	def filter(f: ((Equipo,Tarea) => Boolean)) = if (f(equipo,tarea)) this else Fallida(equipo,tarea,"El equipo no cumplio la condicion")
	def flatMap(f: (Equipo => ResultadoMision)) = f(equipo)
	def fold[T](e: (Equipo => T))(f: (Equipo => T)): T = f(equipo)
}

case class Fallida(val equipo: Equipo,tarea:Tarea, descripcion: String) extends ResultadoMision {
	def map(f: (Equipo => Equipo)) = this
	def map(f: ((Tarea,Equipo) => (Tarea,Equipo))): ResultadoMision = this
	def filter(t: ((Equipo,Tarea) => Boolean)) = this
	def flatMap(f: (Equipo => ResultadoMision)) = this
	def fold[T](e: (Equipo => T))(f: (Equipo => T)): T = e(equipo)
}

case class Mision(tareas:List[Tarea], recompensa: (Equipo => Equipo)) {

	def serRealizadaPor(equipo:Equipo):ResultadoMision =  {
	  val estadoInicial:ResultadoMision = Exitosa(equipo,null)
		val resultado = tareas.foldLeft(estadoInicial)((e,tarea) => tarea.serRealizadaPor(e))
		resultado.map (this.recompensa)
	}

	//def puedeRealizarMision(e:Equipo)={
	  
	//}
}


case class Tarea(facilidad:((Heroe, Equipo) => Int), efectoSobre:((Heroe,Equipo)=> Equipo), condicion:(Equipo => Boolean) = (_ => true)) {
  
  def serRealizadaPor(equipoEnMision:ResultadoMision):ResultadoMision = {
      equipoEnMision.map(asignar)
                    .filter { (equipo,tarea) => tarea.condicion(equipo)}
                    .flatMap { e => equipoConMejorHeroe(e) }
  }
      
  val asignar:((Tarea,Equipo) => (Tarea,Equipo)) = { (_,e) => (this,e) }
  
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
    serRealizadaPor(Exitosa(equipo,this)) match{
      case Exitosa(x,t) => x
      case _ => equipo
    }
  }
  
}


package object tareas {
  
  type Efecto = (Heroe,Equipo)=> Equipo
  type Facilidad = (Heroe, Equipo) => Int
  type Condicion = (Equipo => Boolean)
  
  // efectos
  def efectoMonstruo(fuerzaMonstruo:Int):Efecto = { (heroe,equipo) =>
    if(heroe.stats().fuerza < 20){
      afectarA(heroe,equipo,List(HP(-fuerzaMonstruo)))
    }
    equipo
  }
  
  def efectoForzarPuerta:Efecto = { (heroe,equipo) =>
    heroe.trabajo match {
      case Mago => equipo
      case Ladron => equipo
      case _ => afectarA(heroe,equipo,List(Fuerza(+1),HP(-5)))
    }
  }
  
  def efectoRobarTalisman(talisman:Item):Efecto = { (_,equipo) => equipo.obtenerItem(talisman) }
  
  def afectarA(heroe:Heroe,equipo:Equipo,variaciones:List[Stat]):Equipo = {
    equipo.reemplazarMiembro(heroe, heroe.modificarStats(variaciones:_*))
  }
  
  // facilidades
  def facilidadMonstruo:Facilidad = { (_,equipo) =>
    equipo.lider match {
      case Some(h) => h.trabajo match {
        case Guerrero => 20
        case _ => 10
      }
      case None => 10
    }
  }
  
  def facilidadPuerta:Facilidad = { (heroe,equipo) =>
    heroe.stats().inteligencia + 10 * equipo.obtenerLosQueSean(Ladron).length
  }
  
  def facilidadTalisman:Facilidad = { (heroe,_) =>
    heroe.stats().velocidad
  }
  
  // condiciones
  def condicionTalisman:Condicion = { equipo =>
    equipo.lider.map { _.trabajo } match {
      case Some(Ladron) => true
      case _ => false
    }
  }
  
}
