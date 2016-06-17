package domain

case class Heroe(statsBase:Stats,inventario:Inventario = Inventario(),trabajo:Trabajo = SinTrabajo) { //hay que validar que no sean menores a 1.podemos ponerlos x default como 1 o tirar excepcion

  def fuerzaBase = statsBase.fuerza
  def velocidadBase = statsBase.velocidad
  def inteligenciaBase = statsBase.inteligencia
  def hpBase = statsBase.hp
  
  
  
  def modificarStats(variaciones:Stat*) = {
    copy(statsBase = statsBase.actualizarSegun(variaciones.toList))
  }
  
  def equipar(item:Item):Heroe = {
    if(item.cumpleCondicion(this)){
      return inventario.agregarA(this,item)
    }
    this
  }
  
  def stats():Stats = inventario.efectoSobre(trabajo.efectoSobre(this)).statsBase
  
  
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


abstract class Stat(valor:Int)
case class HP(valor:Int) extends Stat(valor)
case class Fuerza(valor:Int) extends Stat(valor)
case class Velocidad(valor:Int) extends Stat(valor)
case class Inteligencia(valor:Int) extends Stat(valor)





case class Inventario(items:List[Item] = List()){
  
  def agregarA(heroe:Heroe,item:Item):Heroe = {
    actualizarInventario(heroe,
      item.tipo match {
        case Talisman => item::items
        case Mano(false) => agregarItemDeMano(item)
        case Mano(true) => agregarItemDeMano(item)
        case _ => reemplazarOAgregar(item,items)
      })
  }
  
  def actualizarInventario(heroe:Heroe,itemsActualizados:List[Item]):Heroe = {
    heroe.copy(inventario = copy(items = itemsActualizados))
  }
  
  def agregarItemDeMano(item:Item):List[Item] = {
    var itemsActualizados = items.filterNot { _.tipo == Mano(true) }
    if(items.count { _.tipo == Mano(false) } == 2){
      return reemplazarOAgregar(item,itemsActualizados)
    }
    item::itemsActualizados
  }
  
  def soloUnItemDeMano(items:List[Item]):List[Item] = {
    (items.find { _.tipo == Mano(false) }).get::items.filterNot { _.tipo == Mano(false) }
  }
  
  def reemplazarOAgregar(item:Item,items:List[Item]):List[Item] = {
    item::items.filterNot { _.tipo == item.tipo }
  }
  
  def efectoSobre(heroe:Heroe):Heroe = {
    items.foldLeft(heroe)((heroe,item) => item.afectarA(heroe))
  }
  
  def tiene(item:Item) = items.contains(item)
  
}


case class Item(tipo:TipoItem,condicion:(Heroe => Boolean),efectoSobre:(Heroe => List[Stat])) {
  
  def cumpleCondicion(heroe:Heroe):Boolean = {
    condicion(heroe)
  }
  
  def afectarA(heroe:Heroe):Heroe = heroe.modificarStats(efectoSobre(heroe):_*)
  
}

class TipoItem
case object Cabeza extends TipoItem
case object Torso extends TipoItem
case class Mano(dosManos:Boolean = false) extends TipoItem
case object Talisman extends TipoItem






abstract class Trabajo {
  def statsAfectados():List[Stat]
  def efectoSobre(heroe:Heroe):Heroe
}

case class TrabajoEfectivo(val statPrincipal:Stat,val otrosStats:Stat*) extends Trabajo{
  
  override def statsAfectados = statPrincipal::otrosStats.toList
  
  override def efectoSobre(heroe:Heroe):Heroe = heroe.modificarStats(statsAfectados:_*)
}

case object SinTrabajo extends Trabajo {
  def statsAfectados() = List()
  def efectoSobre(heroe:Heroe):Heroe = heroe
}







