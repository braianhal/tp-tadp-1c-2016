package domain

class Mision(tareas: Tarea, recompensa: (Equipo => Equipo)) {
  
}

case class Tarea(facilidad:((Heroe, Equipo) => Int), condicion:(Equipo => Boolean), efectoSobre:((Heroe,Equipo)=> Equipo)) {
  
  def realizar(equipo:Equipo):Option[Equipo] = {
 //   if (condicion(equipo))
      mejorHeroeParaTarea(equipo).map { heroe => efectoSobre(heroe,equipo)  }
    
 //   else 
      //dar failure
  }
  
  def mejorHeroeParaTarea(equipo:Equipo):Option[Heroe] = {
    equipo.mejorHeroeSegun { heroe => this.facilidad(heroe,equipo) } 
  }

  def serRealizadaPor(equipo:Equipo) = {
    realizar(equipo)match{
      case Some(x) => x
      case _ => equipo
    }
  }
  
}
