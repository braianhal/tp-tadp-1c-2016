package domain

case class EstadoMision(equipo:Equipo,tarea:Tarea=null)

trait ResultadoMision {
	def map(f: (EstadoMision => EstadoMision)): ResultadoMision
	def filter(f: (EstadoMision => Boolean)): ResultadoMision
	def flatMap(f: (EstadoMision => ResultadoMision)): ResultadoMision
	def fold[T](e: (EstadoMision => T))(f: (EstadoMision => T)): T
}

case class Exitosa(estado:EstadoMision) extends ResultadoMision {
	def map(f: (EstadoMision => EstadoMision)) = Exitosa(f(estado))
	def filter(f: (EstadoMision => Boolean)) = if (f(estado)) this else Fallida(estado,"El equipo no cumplio la condicion")
	def flatMap(f: (EstadoMision => ResultadoMision)) = f(estado)
	def fold[T](e: (EstadoMision => T))(f: (EstadoMision => T)): T = f(estado)
}

case class Fallida(estado:EstadoMision, descripcion: String) extends ResultadoMision {
	def map(f: (EstadoMision => EstadoMision)): ResultadoMision = this
	def filter(t: (EstadoMision => Boolean)) = this
	def flatMap(f: (EstadoMision => ResultadoMision)) = this
	def fold[T](e: (EstadoMision => T))(f: (EstadoMision => T)): T = e(estado)
}

case class Mision(tareas:List[Tarea], recompensa: (Equipo => Equipo)) {

	def serRealizadaPor(equipo:Equipo):ResultadoMision =  {
	  realizar(Exitosa(EstadoMision(equipo)))
	}

	def realizar(r:ResultadoMision):ResultadoMision={   
		val resultado = tareas.foldLeft(r)((e,tarea) => tarea.serRealizadaPor(e))
		resultado.map { obtenerRecompensa }
	}
	
	val obtenerRecompensa:(EstadoMision => EstadoMision) = { estado => estado.copy(equipo = this.recompensa(estado.equipo)) }

}


case class Tarea(facilidad:((Heroe, Equipo) => Int), efectoSobre:((Heroe,Equipo)=> Equipo), condicion:(Equipo => Boolean) = (_ => true)) {
  
  def serRealizadaPor(equipoEnMision:ResultadoMision):ResultadoMision = {
      equipoEnMision.map(asignar)
                    .filter { estado => estado.tarea.condicion(estado.equipo) }
                    .flatMap { estado => equipoConMejorHeroe(estado) }
  }
      
  val asignar:(EstadoMision => EstadoMision) = { estado => estado.copy(tarea = this) }
  
  def equipoConMejorHeroe(estado:EstadoMision):ResultadoMision = {
    this.mejorHeroeParaTarea(estado.equipo) match{
      case Some(heroe) => Exitosa(aplicarEfectoSobre(heroe,estado))
      case _ => Fallida(estado,"Nadie puede realizarla")
    }
  }
  
  def mejorHeroeParaTarea(equipo:Equipo):Option[Heroe] = {
    equipo.mejorHeroeSegun { heroe => this.facilidad(heroe,equipo) } 
  }
  
  def aplicarEfectoSobre(heroe:Heroe,estadoMision:EstadoMision):EstadoMision = {
    estadoMision.copy(equipo = efectoSobre(heroe,estadoMision.equipo))
  }
    
  //--------para test---------
  def serRealizadaPorTest(equipo:Equipo) = {   
    serRealizadaPor(Exitosa(EstadoMision(equipo,this))) match{
      case Exitosa(EstadoMision(e,_)) => e
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
