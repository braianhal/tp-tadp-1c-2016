package domain

class Taberna(misiones:List[Mision]) {
  
  def elegirMision(equipo:Equipo,criterio:((Equipo,Equipo)=>Boolean),mision1:Mision,mision2:Mision):Option[Equipo] = {
    val e1 = mision1.serRealizadaPor(equipo)
    val e2 = mision1.serRealizadaPor(equipo)

     val r1:Option[Equipo] = e1 match{
	    case Exitosa(e,t) => Some(e)
	    case Fallida(e,t,_) => None
	  } 
    val r2 = e2 match{
	    case Exitosa(e,t) => Some(e)
	    case Fallida(e,t,_) => None
	  }
    
    (r1,r2)match{
      case (Some(x),None) => Some(x)
      case (None,Some(x)) => Some(x)
      case (Some(x),Some(y)) => Some(x)//aplicar(x,y)   esto cambia
    }
  }
  
}