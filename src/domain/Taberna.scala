package domain

class Taberna(misiones:List[Mision]) {
  
  type Criterio = (Equipo,Equipo) => Boolean
  type MisionHecha = (Mision,ResultadoMision)
  
  def elegirMision(equipo:Equipo, criterio:Criterio):Option[Mision] = {
    val resultados = misiones.map { mision => (mision,mision.serRealizadaPor(equipo)) }
    mejorSegun(criterio,resultados)
  }
  
  def mejorSegun(criterio:Criterio,resultados:List[MisionHecha]):Option[Mision] = {
    resultados match {
      case List() => None
      case x::xs => Some(xs.foldLeft(x)(elMejor(criterio))._1)
    }
  }

  def elMejor(criterio:Criterio)(m1:MisionHecha,m2:MisionHecha):MisionHecha = {
    (m1._2,m2._2) match {
      case (Exitosa(e1,_),Exitosa(e2,_)) => if(criterio(e1,e2)) m1 else m2
      case (Fallida(_,_,_),Exitosa(_,_)) => m2
      case _ => m1
    }
  }
   
  def entrenar(equipo:Equipo):ResultadoMision={
    val estadoInicial:ResultadoMision = Exitosa(equipo,null)
    misiones.foldLeft(estadoInicial){(r:ResultadoMision,m:Mision)=>
      m.realizar(r)
      }

 
  }

  }
  
  /*
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
*/  


  
