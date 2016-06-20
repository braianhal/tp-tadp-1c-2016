package domain

case class Equipo(nombre:String,pozo:Int,heroes:List[Heroe]) {
  
  def ordenarHeroesSegun(cuantificador:(Heroe => Int)) = heroes.sortBy{ cuantificador(_) }.reverse
  
  def mejorHeroeSegun(cuantificador:(Heroe => Int)):Option[Heroe] = ordenarHeroesSegun(cuantificador).headOption
  
  def obtenerItem(item:Item):Equipo = {
    alQueMasLeSirve(item) match {
      case Some(heroe) => darleItemA(heroe,item)
      case None => vender(item)
    }
  }
  
  def obtenerMiembro(nuevoMiembro:Heroe):Equipo = copy(heroes = nuevoMiembro::heroes)
  
  def reemplazarMiembro(actual:Heroe,nuevo:Heroe):Equipo = copy(heroes = nuevo::heroes.filterNot { _ == actual })
  
  def lider:Option[Heroe] = {
    ordenarHeroesSegun { _.valorStatPrincipal} match {
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
  
  def alQueMasLeSirve(item:Item):Option[Heroe] = {
    heroes.filter { _.leSirve(item) } match {
      case List() => None
      case heroesBeneficiados => Some(heroesBeneficiados.maxBy { _.beneficioDe(item) })
    }
  }
  
  def darleItemA(heroe:Heroe,item:Item):Equipo = reemplazarMiembro(heroe,heroe.equipar(item))
  
  def vender(item:Item):Equipo = copy(pozo = pozo + item.valor)
  
  //para tests
  def cantidadMiembros = heroes.length
  def tieneA(heroe:Heroe) = heroes.contains(heroe)
  def obtenerLosQueSean(trabajo:Trabajo) = heroes.filter { _ es trabajo }
  
}