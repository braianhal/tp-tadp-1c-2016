package domain

case class Heroe(statsBase:Stats,inventario:Inventario = Inventario()) { //hay que validar que no sean menores a 1.podemos ponerlos x default como 1 o tirar excepcion

  val trabajo:Trabajo = null //FIXME

  
  def modificarStats(variaciones:Stat*) = 
    copy(statsBase.actualizarSegun(variaciones.toList))
  
  
  def equipar(item:Item):Heroe = {
    if(item.cumpleCondicion(this)){
      return inventario.agregarA(this,item)
    }
    this
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

case class Inventario(items:List[Item] = List()){
  
  def agregarA(heroe:Heroe,item:Item):Heroe = {
    actualizarInventario(heroe,
      item.tipo match {
        case Talisman => item::items
        case Mano(_) => agregarItemDeMano(item)
        case _ => reemplazarOAgregar(item,items)
      })
  }
  
  def actualizarInventario(heroe:Heroe,itemsActualizados:List[Item]):Heroe = {
    heroe.copy(inventario = copy(items = itemsActualizados))
  }
  
  def agregarItemDeMano(item:Item):List[Item] = {
    var itemsActualizados = items.filterNot { i => i.tipo == Mano(true) }
    if(items.count { i => i.tipo == Mano(false) } == 2){
      return reemplazarOAgregar(item,itemsActualizados)
    }
    item::itemsActualizados
  }
  
  //def soloUnItemDeMano(items:List[Item]):List[Item] = {
  //  (items.find { i => i.tipo == Mano(false) }).get::items.filterNot { i => i.tipo == Mano(false) }
  //}
  
  def reemplazarOAgregar(item:Item,items:List[Item]):List[Item] = {
    item::items.filterNot { i => i.tipo == item.tipo }
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

class TipoItem
case object Cabeza extends TipoItem
case object Torso extends TipoItem
case class Mano(dosManos:Boolean = false) extends TipoItem
case object Talisman extends TipoItem
