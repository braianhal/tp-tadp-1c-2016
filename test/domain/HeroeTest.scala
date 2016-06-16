package domain

import org.junit.Before
import org.junit.Test
import org.junit.Assert._

class HeroeTest {
  
  var heroe:Heroe = null
  
  @Before
  def setup() = {
     heroe = Heroe(Stats(10,20,30,40))
  }

  @Test
  def heroeSeCreaConStatsBase() = {
    assertEquals(heroe.statsBase, Stats(10,20,30,40))
  }
  
  @Test
  def aumentarHpDeHeroe() = {
    val heroeMasFuerte:Heroe = heroe.modificarStats((HP,20))
    assertEquals(heroe.statsBase, Stats(10,20,30,40))
    assertEquals(heroeMasFuerte.statsBase, Stats(30,20,30,40))
  }
  
  @Test
  def aumentarFuerzaDeHeroe() = {
    val heroeMasFuerte:Heroe = heroe.modificarStats((Fuerza,20))
    assertEquals(heroe.statsBase, Stats(10,20,30,40))
    assertEquals(heroeMasFuerte.statsBase, Stats(10,40,30,40))
  }
  
  @Test
  def aumentarVelocidadDeHeroe() = {
    val heroeMasFuerte:Heroe = heroe.modificarStats((Velocidad,20))
    assertEquals(heroe.statsBase, Stats(10,20,30,40))
    assertEquals(heroeMasFuerte.statsBase, Stats(10,20,50,40))
  }
  
  @Test
  def aumentarInteligenciaDeHeroe() = {
    val heroeMasFuerte:Heroe = heroe.modificarStats((Inteligencia,20))
    assertEquals(heroe.statsBase, Stats(10,20,30,40))
    assertEquals(heroeMasFuerte.statsBase, Stats(10,20,30,60))
  }
}