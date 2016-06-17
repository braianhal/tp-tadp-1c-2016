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
  
  
  //Stats
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
  
  
  //Trabajo
  @Test
  def noTenerTrabajoNoCambiaLosStats(){
    val heroeSinTrabajo = heroe.asignarTrabajo(SinTrabajo)
    assertEquals(heroeSinTrabajo.statsBase,heroeSinTrabajo.stats())
  }
  
  @Test
  def heroeConTrabajoDeGuerrero(){
    val heroeConTrabajo = heroe.asignarTrabajo(Guerrero)
    assertEquals(heroeConTrabajo.hpBase+10,heroeConTrabajo.stats().hp)
    assertEquals(heroeConTrabajo.fuerzaBase+15,heroeConTrabajo.stats().fuerza)
    assertEquals(heroeConTrabajo.inteligenciaBase-10,heroeConTrabajo.stats().inteligencia)
    assertEquals(heroeConTrabajo.velocidadBase,heroeConTrabajo.stats().velocidad)
  }
  
  @Test
  def heroeConTrabajoDeMago(){
    val heroeConTrabajo = heroe.asignarTrabajo(Mago)
    assertEquals(heroeConTrabajo.hpBase,heroeConTrabajo.stats().hp)
    assertEquals(1,heroeConTrabajo.stats().fuerza) // daría 0, pero el mínimo es 1
    assertEquals(heroeConTrabajo.inteligenciaBase+20,heroeConTrabajo.stats().inteligencia)
    assertEquals(heroeConTrabajo.velocidadBase,heroeConTrabajo.stats().velocidad)
  }
  
  @Test
  def heroeConTrabajoDeLadron(){
    val heroeConTrabajo = heroe.asignarTrabajo(Ladron)
    assertEquals(heroeConTrabajo.hpBase-5,heroeConTrabajo.stats().hp)
    assertEquals(heroeConTrabajo.fuerzaBase,heroeConTrabajo.stats().fuerza) // daría 0, pero el mínimo es 1
    assertEquals(heroeConTrabajo.inteligenciaBase,heroeConTrabajo.stats().inteligencia)
    assertEquals(heroeConTrabajo.velocidadBase+10,heroeConTrabajo.stats().velocidad)
  }
  
  @Test
  def quedarseSinTrabajo(){
    val heroeConTrabajo = heroe.asignarTrabajo(Ladron)
    val heroeSinTrabajo = heroeConTrabajo.perderTrabajo
    
    assertNotEquals(heroeConTrabajo.statsBase,heroeConTrabajo.stats())
    assertEquals(heroeSinTrabajo.statsBase,heroeSinTrabajo.stats())
  }
  
  
  //Inventario
  @Test
  def equiparCascoVikingo(){
    val otroHeroeConCasco = superHeroe.equipar(cascoVikingo)
    assertEquals(superHeroe.hpBase + 10,otroHeroeConCasco.stats().hp)
  }
  
  @Test
  def noCumpleCondicionParaEquiparCasco(){
    val casiHeroeSinCasco = casiHeroe.equipar(cascoVikingo)
    assertFalse(casiHeroeSinCasco.inventario.tiene(cascoVikingo))
    assertEquals(casiHeroe.statsBase,casiHeroe.stats())
  }
  
}