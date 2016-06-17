package domain

import org.junit.Before
import org.junit.Test
import org.junit.Assert._

class HeroeTest {
  
  var heroe:Heroe = null
  var superHeroe:Heroe = null
  var casiHeroe:Heroe = null
  var cascoVikingo:Item = null
  
  @Before
  def setup() = {
     heroe = Heroe(Stats(10,20,30,40))
     superHeroe = Heroe(Stats(100,100,100,100))
     casiHeroe = Heroe(Stats(5,5,5,5))
     cascoVikingo = Item(Cabeza,(_.fuerzaBase > 30),(_ => List(HP(+10))))
  }
  

  @Test
  def heroeSeCreaConStatsBase() = {
    assertEquals(heroe.statsBase, Stats(10,20,30,40))
  }
  
  @Test
  def aumentarHpDeHeroe() = {
    val heroeMasFuerte:Heroe = heroe.modificarStats(HP(20))
    assertEquals(heroe.statsBase, Stats(10,20,30,40))
    assertEquals(heroeMasFuerte.statsBase, Stats(30,20,30,40))
  }
  
  @Test
  def aumentarFuerzaDeHeroe() = {
    val heroeMasFuerte:Heroe = heroe.modificarStats(Fuerza(20))
    assertEquals(heroe.statsBase, Stats(10,20,30,40))
    assertEquals(heroeMasFuerte.statsBase, Stats(10,40,30,40))
  }
  
  @Test
  def aumentarVelocidadDeHeroe() = {
    val heroeMasVeloz:Heroe = heroe.modificarStats(Velocidad(20))
    assertEquals(heroe.statsBase, Stats(10,20,30,40))
    assertEquals(heroeMasVeloz.statsBase, Stats(10,20,50,40))
  }
  
  @Test
  def aumentarInteligenciaDeHeroe() = {
    val heroeMasInteligente:Heroe = heroe.modificarStats(Inteligencia(20))
    assertEquals(heroe.statsBase, Stats(10,20,30,40))
    assertEquals(heroeMasInteligente.statsBase, Stats(10,20,30,60))
  }
  
  @Test
  def aumentarVariosStatsALaVez(){
    val nuevoHeroe:Heroe = heroe.modificarStats(Inteligencia(20),Fuerza(30),Velocidad(5))
    assertEquals(heroe.statsBase, Stats(10,20,30,40))
    assertEquals(nuevoHeroe.statsBase, Stats(10,50,35,60))
  }
  
  @Test
  def disminuirStat(){
    val nuevoHeroe:Heroe = heroe.modificarStats(Fuerza(-10))
    assertEquals(heroe.statsBase, Stats(10,20,30,40))
    assertEquals(nuevoHeroe.statsBase, Stats(10,10,30,40))
  }
  
  @Test
  def heroeNoAceptaStatsNegativos() = {
    val heroeMenosFuerte:Heroe = heroe.modificarStats(HP(-15420),Fuerza(-77777720),Velocidad(-1520),Inteligencia(-200))
    assertEquals(heroe.statsBase, Stats(10,20,30,40))
    assertEquals(heroeMenosFuerte.statsBase, Stats(1,1,1,1))
  }
  
  
  @Test
  def equiparCascoVikingo(){
    val otroHeroeConCasco = superHeroe.equipar(cascoVikingo)
    assertEquals(otroHeroeConCasco.stats().hp, superHeroe.hpBase + 10)
  }
  
  @Test
  def noCumpleCondicionParaEquiparCasco(){
    val casiHeroeSinCasco = casiHeroe.equipar(cascoVikingo)
    assertFalse(casiHeroeSinCasco.inventario.tiene(cascoVikingo))
    assertEquals(casiHeroe.statsBase,casiHeroe.stats())
  }
  
}