package domain


case class Heroe(statsBase:Stats, inventario:Inventario = Inventario(), trabajo:Trabajo = SinTrabajo) { //hay que validar que no sean menores a 1.podemos ponerlos x default como 1 o tirar excepcion

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
  
  def cambiarDeTrabajo(trabajoNuevo:Trabajo) = copy(trabajo = trabajoNuevo)
  
  def es(unTrabajo:Trabajo) = trabajo == unTrabajo
  
  def valorStatPrincipal = trabajo.valorStatPrincipal(this)
  
  def leSirve(item:Item) = beneficioDe(item) > 0
  
  def beneficioDe(item:Item) = this.equipar(item).valorStatPrincipal - this.valorStatPrincipal
  
}



case class Stats(hp:Int,fuerza:Int,velocidad:Int,inteligencia:Int){
  
  def actualizarSegun(variaciones:List[Stat]) = {
    variaciones.foldLeft(this)(aplicarVariacion)
  }
  
  // TODO se podría hacer que reciba una función genérica actualizarStat (para que quede mejor en los efectos de los items)
  def aplicarVariacion(stats:Stats,variacion:Stat):Stats = {
    var nuevosStats:Stats = stats.copy()
    variacion match {
        case HP(x) => nuevosStats = nuevosStats.copy(hp = actualizarStat(this.hp,x))
        case Fuerza(x) => nuevosStats = nuevosStats.copy(fuerza = actualizarStat(this.fuerza,x))
        case Velocidad(x) => nuevosStats = nuevosStats.copy(velocidad = actualizarStat(this.velocidad,x))
        case Inteligencia(x) => nuevosStats = nuevosStats.copy(inteligencia = actualizarStat(this.inteligencia,x))
      }
    nuevosStats
  }
  
  def actualizarStat(valorAnterior:Int,variacion:Int) = Math.max(1,variacion + valorAnterior)
  
  def toList = List(HP(hp),Fuerza(fuerza),Velocidad(velocidad),Inteligencia(inteligencia))
  
  def valorDe(stat:Stat) = this.toList.find { _.getClass() == stat.getClass() }.get.valor
  
  def todosCon(valor:Int) = this.copy(valor,valor,valor,valor).toList
  
  def mapValores(f:Int=>Int):Stats = {
    copy(hp = f(hp),fuerza = f(fuerza), inteligencia = f(inteligencia),velocidad = f(velocidad))
  }
  
  def modificar(unStat:Stat,nuevoValor:Int):Stat = 
    unStat match {
      case HP(x) => HP(nuevoValor)
      case Fuerza(x) => Fuerza(nuevoValor)
      case Velocidad(x) => Velocidad(nuevoValor)
      case Inteligencia(x) => Inteligencia(nuevoValor)
    }
}


abstract class Stat(val valor:Int)
case class HP(v:Int) extends Stat(v)
case class Fuerza(v:Int) extends Stat(v)
case class Velocidad(v:Int) extends Stat(v)
case class Inteligencia(v:Int) extends Stat(v)



case class Inventario(cabeza:Item = null, torso:Item = null, manos:List[Item] = List(), talismanes:List[Item] = List()){
  
  def agregarA(heroe:Heroe,item:Item):Inventario = {
      item.tipo match {
        case Talisman => copy(talismanes = item::talismanes)
        case Mano(false) => copy(manos = agregarItemDeMano(item))
        case Mano(true) => copy(manos = List(item))
        case Cabeza => copy(cabeza = item)
        case Torso => copy(torso = item)
      }
  }
  
  def agregarItemDeMano(item:Item):List[Item] = {
    if (manos.length == 1 && manos.head.tipo == Mano(true))
      return List(item)
    
    if(manos.length == 2)
      return reemplazarItem(item,manos)
 
    item::manos
  }
  
  def reemplazarItem(item:Item, manos:List[Item]):List[Item] = {
    item::manos.drop(1)  //TODO mejorar
  }
  
  
  def efectoSobre(heroe:Heroe):Heroe = {
    todosLosItems.foldLeft(heroe)((heroe,item) => item.afectarA(heroe))
  }
  
  def todosLosItems:List[Item] = {
    var items = manos ++ talismanes
    if (cabeza != null)
      items = cabeza::items
    if (torso != null)
      items = torso::items
    items
  }
  
  def tiene(item:Item) = todosLosItems.contains(item)
  
  def cantidadItems = todosLosItems.length
}



