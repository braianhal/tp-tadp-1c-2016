package domain


case class Heroe(statsBase:Stats,inventario:Inventario = Inventario(),trabajo:Trabajo = SinTrabajo) { //hay que validar que no sean menores a 1.podemos ponerlos x default como 1 o tirar excepcion

  def fuerzaBase = statsBase.fuerza
  def velocidadBase = statsBase.velocidad
  def inteligenciaBase = statsBase.inteligencia
  def hpBase = statsBase.hp
  
  def modificarStats(variaciones:Stat*) = {
    copy(statsBase = statsBase.actualizarSegun(variaciones.toList))
  }

  def stats():Stats = inventario.efectoSobre(trabajo.efectoSobre(this)).statsBase
  
  def equipar(item:Item):Heroe = {
    if(item.cumpleCondicion(this)){
      return copy(inventario = inventario.agregarA(this,item))
    }
    this
  }
  
  def asignarTrabajo(trabajoNuevo:Trabajo) = copy(trabajo = trabajoNuevo)
  
  def perderTrabajo = asignarTrabajo(SinTrabajo)
  
  def es(unTrabajo:Trabajo) = trabajo == unTrabajo
  
  def valorStatPrincipal = trabajo.valorStatPrincipal(this)
  
  def leSirve(item:Item) = beneficioDe(item) > 0
  
  def beneficioDe(item:Item) = this.equipar(item).valorStatPrincipal - this.valorStatPrincipal
  
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
  
  def toList = List(HP(hp),Fuerza(fuerza),Velocidad(velocidad),Inteligencia(inteligencia))
  
  def valorDe(stat:Stat) = this.toList.find { _.getClass() == stat.getClass() }.get.valor
}


abstract class Stat(val valor:Int)
case class HP(v:Int) extends Stat(v)
case class Fuerza(v:Int) extends Stat(v)
case class Velocidad(v:Int) extends Stat(v)
case class Inteligencia(v:Int) extends Stat(v)



case class Inventario(items:List[Item] = List()){
  
  def agregarA(heroe:Heroe,item:Item):Inventario = {
    actualizarInventario(heroe,
      item.tipo match {
        case Talisman => item::items
        case Mano(false) => agregarItemDeMano(item)
        case Mano(true) => item::sinItemsDeMano
        case _ => reemplazarOAgregar(item,items)
      })
  }
  
  def actualizarInventario(heroe:Heroe,itemsActualizados:List[Item]):Inventario = {
    copy(items = itemsActualizados)
  }
  
  def agregarItemDeMano(item:Item):List[Item] = {
    var itemsActualizados = items.filterNot { _.tipo == Mano(true) } 
    
    if(items.count { _.tipo == Mano(false) } == 2){
      return reemplazarOAgregar(item,itemsActualizados)
    }

    item::itemsActualizados
  }
  
  def sinItemsDeMano:List[Item] = items.filterNot { i => i.tipo == Mano(false) || i.tipo == Mano(true) }
  
  def reemplazarOAgregar(item:Item,items:List[Item]):List[Item] = {
    item::items.drop(1)  //TODO mejorar
  }
  
  
  def efectoSobre(heroe:Heroe):Heroe = {
    items.foldLeft(heroe)((heroe,item) => item.afectarA(heroe))
  }
  
  def tiene(item:Item) = items.contains(item)
  
  def cantidadItems = items.length
}



