package domain

import org.junit.Before
import org.junit.Test
import org.junit.Assert._

class HeroeTest {
  
  var heroe:Heroe = null
  
  @Before
  def setup() = {
     heroe = Heroe(Stats(10,10,20,30))
  }

  @Test
  def heroeSeCreaConStatsBase() = {
    assertEquals(heroe.statsBase, Stats(10,10,20,30))
  }
  
  @Test
  def aumentarFuerzaDeHeroe() = {
    val heroeMasFuerte:Heroe = heroe.modificarStats((Fuerza,20))
    assertEquals(heroe.statsBase, Stats(10,10,20,30))
    assertEquals(heroeMasFuerte.statsBase, Stats(10,30,20,30))
  }
}