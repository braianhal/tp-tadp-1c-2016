package domain

case class Item(tipo:TipoItem, efectoSobre:efectos.EfectoHeroe, condicion:condiciones.CondicionHeroe = (_ => true)) {
  
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
}

package object condiciones {
  type CondicionHeroe = Heroe => Boolean
  
  def fuerzaBaseMayorA(valor:Int)(heroe:Heroe) =
    heroe.fuerzaBase > valor
    
  def inteligenciaBaseMayorA(valor:Int)(heroe:Heroe) =
    heroe.inteligenciaBase > valor
}