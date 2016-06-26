package domain

case class MisionHecha(mision:Mision,resultado:ResultadoMision)

case class Taberna(misiones:List[Mision]) {
  
  type Criterio = (Equipo,Equipo) => Boolean
  
  def elegirMision(equipo:Equipo, criterio:Criterio):Option[Mision] = {
    val resultados = misiones.map { mision => MisionHecha(mision,mision.serRealizadaPor(equipo)) }
    mejorSegun(criterio,resultados)
  }
  
  def mejorSegun(criterio:Criterio,resultados:List[MisionHecha]):Option[Mision] = {
    resultados match {
      case List() => None
      case x::xs => exitosaONinguna(xs.foldLeft(x)(elMejor(criterio)))
    }
  }
  
  def exitosaONinguna(misionHecha:MisionHecha):Option[Mision] = {
    misionHecha.resultado match {
      case Exitosa(_,_) => Some(misionHecha.mision)
      case _ => None
    }
  }

  def elMejor(criterio:Criterio)(m1:MisionHecha,m2:MisionHecha):MisionHecha = {
    (m1.resultado,m2.resultado) match {
      case (Exitosa(e1,_),Exitosa(e2,_)) => if(criterio(e1,e2)) m1 else m2
      case (Fallida(_,_,_),Exitosa(_,_)) => m2
      case _ => m1
    }
  }
  
  def entrenar(equipo:Equipo,criterio:Criterio):Equipo = {
    realizarMejorMision(equipo,criterio,misiones)
  }
  
  def realizarMejorMision(equipo:Equipo,criterio:Criterio,misiones:List[Mision]):Equipo = {
     copy(misiones = misiones).elegirMision(equipo,criterio) match {
      case Some(mision) => realizarMejorMision(equipo.realizar(mision)._1, criterio, sinMision(mision,misiones))
      case _ => equipo
    }
  }
  
  def sinMision(mision:Mision,misiones:List[Mision]):List[Mision] = misiones.filterNot { m => m == mision }
  
} 


  
