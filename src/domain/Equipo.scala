package domain

case class Equipo(nombre:String,pozo:Int,heroes:List[Heroe]) {
  
  def mejorHeroeSegun(cuantificador:(Heroe => Int)):Option[Heroe] = heroes.sortBy{ cuantificador(_) }.reverse.headOption
  
  def obtenerMiembro(nuevoMiembro:Heroe):Equipo = copy(heroes = nuevoMiembro::heroes)
  
  def reemplazarMiembro(actual:Heroe,nuevo:Heroe):Equipo = copy(heroes = nuevo::heroes.filterNot { _ == actual })
  
  //FIXME no esta andando en todos los casos
  def lider:Option[Heroe] = {
    heroes.sortBy { _.valorStatPrincipal }.reverse match {
      case List() => None
      case List(h) => Some(h)
      case h1::h2::hs => liderEntre(h1,h2)
    }
  }
  
  def liderEntre(heroe:Heroe,otroHeroe:Heroe) = {
    if(heroe.valorStatPrincipal == otroHeroe.valorStatPrincipal){
      None
    }
    else{
      Some(heroe)
    }
  }
  
  def cantidadMiembros = heroes.size
  
  def tieneA(heroe:Heroe) = heroes.contains(heroe)
  
}