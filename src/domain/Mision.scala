package domain

import scala.util.Success
import scala.util.Try
import scala.util.Failure
import scala.util.control.Exception


class Mision(tareas:List[Tarea], recompensa: (Equipo => Equipo)) {
  
  def realizarMision(equipo:Equipo):Equipo =  {
     var team: Try[Equipo]= Success(equipo) 
     team = tareas.foldLeft(team)((e,tarea) => e.map { e1 => tarea.serRealizadaPor(e1)})
     team match{
       case Success(e) => e
       case Failure(e) => equipo  //to do devolver descripcion
   }
 
  }
}

case class Tarea(facilidad:((Heroe, Equipo) => Int), condicion:(Equipo => Boolean), efectoSobre:((Heroe,Equipo)=> Equipo)) {
  
  def realizar(equipo:Equipo):Option[Equipo] = {
     var team:Option[Equipo] = Some(equipo)
      team = verificarCondicion(team)
      val h = team.flatMap {e => this.mejorHeroeParaTarea(e) }
      h.map { heroe => efectoSobre(heroe,equipo)  }
    /* algun dia ...
     for{
        x <- team;
        y <- this.mejorHeroeParaTarea(x);
        z = this.efectoSobre(y,x);
    }   */
  }
  
  def mejorHeroeParaTarea(equipo:Equipo):Option[Heroe] = {
    equipo.mejorHeroeSegun { heroe => this.facilidad(heroe,equipo) } 
  }

  def verificarCondicion(equipo:Option[Equipo]):Option[Equipo] = {
    equipo.filter { equipo => this.condicion(equipo)}
  }
    
  def serRealizadaPor(equipo:Equipo) = {   //TO DO  devolver en forma de Trys (capaz no)
    realizar(equipo)match{
      case Some(x) => x
      case _ => /*equipo*/throw new Throwable("no la realizo")
    }
  }
  
}
