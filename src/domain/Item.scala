package domain

case class Item(tipo:TipoItem,valor:Int, efectoSobre:efectos.EfectoHeroe, condicion:condiciones.CondicionHeroe = (_ => true)) {
  
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


package object efectos {
  type EfectoHeroe = Heroe => List[Stat]
  
  def modificarHP(delta:Int)(heroe:Heroe) =
    List(HP(delta))
  
  def modificarInteligencia(delta:Int)(heroe:Heroe) =
    List(Inteligencia(delta))
  
  def modificarVelocidad(delta:Int)(heroe:Heroe) =
    List(Velocidad(delta))
    
  def modificarFuerza(delta:Int)(heroe:Heroe) =
    List(Fuerza(delta))
    
  def afectarPorMinimalismo(heroe:Heroe) = {
    val itemsExtra = heroe.inventario.cantidadItems - 1
    List(HP(+50),HP(-10*itemsExtra))
  }
  
  def afectarPorDedicacion(heroe:Heroe) = {
    heroe.stats().todosCon(heroe.valorStatPrincipal / 10)
  }
  
  def efectoBufaloDeAgua(heroe:Heroe) = {
    val stats = heroe.stats()
    if(stats.fuerza > stats.inteligencia) {
      List(Inteligencia(+30))
    }
    else{
      stats.todosCon(+10).filterNot { stat => stat.getClass() == Inteligencia }
    }
  }
  
  def todosLosStatsEn(valorNuevo:Int)(heroe:Heroe):List[Stat] = {
    heroe.stats().mapValores { valor => -(valor - valorNuevo) }.toList
  }
  
  def igualarStats(stat:Stat,otroStat:Stat)(heroe:Heroe) = {
    List(heroe.stats().modificar(otroStat,stat.valor))
  }
  
  def modificarTodos(delta:Int)(heroe:Heroe) = {
    List(HP(delta),Fuerza(delta),Velocidad(delta),Inteligencia(delta))
  }
}

package object condiciones {
  type CondicionHeroe = Heroe => Boolean
  
  def fuerzaBaseMayorA(valor:Int)(heroe:Heroe) =
    heroe.fuerzaBase > valor
    
  def inteligenciaBaseMayorA(valor:Int)(heroe:Heroe) =
    heroe.inteligenciaBase > valor
  
  def aptoParaPalitoMagico(heroe:Heroe) =
    heroe.es(Mago) || heroe.es(Ladron) && inteligenciaBaseMayorA(30)(heroe)
    
  def aptoParaEscudoAntiRobo(heroe:Heroe) =
    !heroe.es(Ladron) && fuerzaBaseMayorA(20)(heroe)
  
}