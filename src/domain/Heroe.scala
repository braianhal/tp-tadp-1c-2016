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
      return inventario.agregarA(this,item)
    }
    this
  }
  
  def asignarTrabajo(trabajoNuevo:Trabajo) = copy(trabajo = trabajoNuevo)
  
  def perderTrabajo = asignarTrabajo(SinTrabajo)
  
  def es(unTrabajo:Trabajo) = trabajo == unTrabajo
  
  def valorStatPrincipal = trabajo.valorStatPrincipal(this)
  
}



// FIXME sospecho que sería mejor tener una lista de tuplas
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
}


abstract class Stat(val valor:Int)
case class HP(v:Int) extends Stat(v)
case class Fuerza(v:Int) extends Stat(v)
case class Velocidad(v:Int) extends Stat(v)
case class Inteligencia(v:Int) extends Stat(v)





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
    var itemsActualizados = items.filterNot { _.tipo == Mano(true) }
    if(items.count { _.tipo == Mano(false) } == 2){
      return reemplazarOAgregar(item,itemsActualizados)
    }
    item::itemsActualizados
  }
  
  def reemplazarOAgregar(item:Item,items:List[Item]):List[Item] = {
    item::items.filterNot { _.tipo == item.tipo }
  }
  
  def efectoSobre(heroe:Heroe):Heroe = {
    items.foldLeft(heroe)((heroe,item) => item.afectarA(heroe))
  }
  
  def tiene(item:Item) = items.contains(item)
  
}


case class Item(tipo:TipoItem,efectoSobre:(Heroe => List[Stat]),condicion:(Heroe => Boolean) = (_ => true),precio:Int = 1) {
  
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
  def valorStatPrincipal(heroe:Heroe):Int
}

class TrabajoEfectivo(val statPrincipal:Stat,val otrosStats:Stat*) extends Trabajo{
  
  def statsAfectados = statPrincipal::otrosStats.toList
  
  def efectoSobre(heroe:Heroe):Heroe = heroe.modificarStats(statsAfectados:_*)
  
  //FIXME deberia devolver el valor del stat principal del trabajo de los stats() del heroe
  def valorStatPrincipal(heroe:Heroe) = 2
  
}

case object Guerrero extends TrabajoEfectivo(Fuerza(+15),HP(+10),Inteligencia(-10))
case object Mago extends TrabajoEfectivo(Inteligencia(+20),Fuerza(-20))
case object Ladron extends TrabajoEfectivo(Velocidad(+10),HP(-5))

//FIXME cambiarlo por un try, option o algo así
case object SinTrabajo extends Trabajo {
  def statsAfectados() = List()
  def efectoSobre(heroe:Heroe):Heroe = heroe
  def valorStatPrincipal(heroe:Heroe) = 0
}







