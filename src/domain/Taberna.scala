package domain

class Taberna(misiones:List[Mision]) {
  
  def elegirMision(equipo:Equipo, criterio:((Equipo,Equipo)=>Boolean),mision1:Mision,mision2:Mision):Option[Mision] = {
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
      case (Some(x),None) => Some(mision1)
      case (None,Some(x)) => Some(mision2)
      case (Some(x),Some(y)) => Some(this.elMejor(x,mision1,y,mision2,criterio))
      case (None,None) => None
    }
  }

  def elMejor(x: Equipo, m1: Mision, y: Equipo,m2: Mision,criterio:((Equipo,Equipo)=>Boolean)):Mision = {
    if (criterio(x,y))
    m1
    else 
    m2
  }

  
}