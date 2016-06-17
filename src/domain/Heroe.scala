package domain

case class Heroe(statsBase:Stats) {
  
  val inventario:Inventario = Inventario()
  val trabajo:Trabajo = null

  
  def modificarStats(variaciones:Stat*) = {
    copy(statsBase.actualizarSegun(variaciones.toList))
  }
  
  def equipar(item:Item) {
    if(item.cumpleCondicion(this)){
      inventario.agregar(item)
    }
  }
  
  
  
}

case class Stats(hp:Int,fuerza:Int,velocidad:Int,inteligencia:Int){
  
  def actualizarSegun(variaciones:List[Stat]) = {
    var nuevosStats:Stats = copy()
    variaciones.foreach { variacion => 
      variacion match {
        case HP(x) => nuevosStats = nuevosStats.copy(hp = actualizarStat(this.hp,x))
        case Fuerza(x) => nuevosStats = nuevosStats.copy(fuerza = actualizarStat(this.fuerza,x))
        case Velocidad(x) => nuevosStats = nuevosStats.copy(velocidad = actualizarStat(this.velocidad,x))
        case Inteligencia(x) => nuevosStats = nuevosStats.copy(inteligencia = actualizarStat(this.inteligencia,x))
      }
    }
    nuevosStats
  }
  
  def actualizarStat(valorAnterior:Int,variacion:Int) = Math.max(1,variacion + valorAnterior)
}

case class Inventario(){
  
  val items:List[Item] = List()
  
  def agregar(item:Item) {
    
  }
  
}

class Trabajo(statPrincipal:Stat,stats:List[Stat]){
  
  
  
}

abstract class Stat(valor:Int)
case class HP(valor:Int) extends Stat(valor)
case class Fuerza(valor:Int) extends Stat(valor)
case class Velocidad(valor:Int) extends Stat(valor)
case class Inteligencia(valor:Int) extends Stat(valor)



case class Item(tipo:TipoItem,efectoSobreHeroe:(Heroe => List[(Stat,Int)]),condicion:(Heroe => Boolean)) {
  
  def cumpleCondicion(heroe:Heroe):Boolean = {
    condicion(heroe)
  }
  
}

trait TipoItem
case object Cabeza
case object Torso
case class Mano(cantidad:Int)
case object Talisman
