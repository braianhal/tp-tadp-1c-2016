package domain

abstract class Trabajo {
  def statsAfectados():List[Stat]
  def efectoSobre(heroe:Heroe):Heroe
  def valorStatPrincipal(heroe:Heroe):Int
}

class TrabajoEfectivo(val statPrincipal:Stat,val otrosStats:Stat*) extends Trabajo{
  
  def statsAfectados = statPrincipal::otrosStats.toList
  
  def efectoSobre(heroe:Heroe):Heroe = heroe.modificarStats(statsAfectados:_*)
  
  def valorStatPrincipal(heroe:Heroe) = heroe.stats().valorDe(statPrincipal)
  
}

case object Guerrero extends TrabajoEfectivo(Fuerza(+15),HP(+10),Inteligencia(-10))
case object Mago extends TrabajoEfectivo(Inteligencia(+20),Fuerza(-20))
case object Ladron extends TrabajoEfectivo(Velocidad(+10),HP(-5))

case object SinTrabajo extends Trabajo {
  def statsAfectados() = List()
  def efectoSobre(heroe:Heroe):Heroe = heroe
  def valorStatPrincipal(heroe:Heroe) = 0
}
