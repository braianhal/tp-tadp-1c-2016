package domain

case class Heroe(statsBase:Stats) {
  
  def modificarStats(variaciones:(Stat,Int)*) = {
    copy(statsBase.actualizarSegun(variaciones.toList))
  }
  
}

case class Stats(hp:Int,fuerza:Int,velocidad:Int,inteligencia:Int){
  
  def actualizarSegun(variaciones:List[(Stat,Int)]) = {
    var nuevosStats:Stats = copy()
    variaciones.foreach { variacion => 
      variacion match {
        case (HP,x) => nuevosStats = nuevosStats.copy(hp = actualizarStat(this.hp,x))
        case (Fuerza,x) => nuevosStats = nuevosStats.copy(fuerza = actualizarStat(this.fuerza,x))
        case (Velocidad,x) => nuevosStats = nuevosStats.copy(velocidad = actualizarStat(this.velocidad,x))
        case (Inteligencia,x) => nuevosStats = nuevosStats.copy(inteligencia = actualizarStat(this.inteligencia,x))
      }
    }
    nuevosStats
  }
  
  def actualizarStat(valorAnterior:Int,variacion:Int) = Math.max(1,variacion + valorAnterior)
}

class Stat
case object HP extends Stat
case object Fuerza extends Stat
case object Velocidad extends Stat
case object Inteligencia extends Stat
