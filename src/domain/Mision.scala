package domain


class Mision(tareas: Tarea, recompensa: (Equipo => Equipo)) {
  
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
    
  def serRealizadaPor(equipo:Equipo) = {   //TO DO  devolver en forma de Trys
    realizar(equipo)match{
      case Some(x) => x
      case _ => equipo
    }
  }
  
}
