package domain

import org.junit.Before
import org.junit.Test
import org.junit.Assert._

class HeroeTest {
  
  val heroe = Heroe(Stats(10,20,30,40))
  val superHeroe = Heroe(Stats(100,100,100,100))
  val casiHeroe = Heroe(Stats(5,5,5,5))
  val cascoVikingo = Item(Cabeza,efectos.modificarHP(+10),condiciones.fuerzaBaseMayorA(30))
  val palitoMagico = Item(Mano(),efectos.modificarInteligencia(+20),
      (heroe => heroe.es(Mago) || (heroe.es(Ladron) && condiciones.inteligenciaBaseMayorA(30)(heroe))))
  val armaduraEleganteSport = Item(Torso,( heroe => efectos.modificarVelocidad(+30)(heroe) ++ efectos.modificarHP(-30)(heroe)) )
  
  @Before
  def setup() = {
     
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

  @Test
  def equiparPalitoMagico(){
    val magoConPalitoMagico = superHeroe.asignarTrabajo(Mago).equipar(palitoMagico)
    assertEquals(superHeroe.inteligenciaBase + 20 + 20, magoConPalitoMagico.stats().inteligencia)
  }
  
  @Test
  def noCumpleCondicionParaEquiparPalitoMagico(){
    val heroeMuggle = superHeroe.equipar(palitoMagico)
    assertFalse(heroeMuggle.inventario.tiene(palitoMagico))
    assertEquals(heroeMuggle.statsBase,heroeMuggle.stats())
  }
  
  @Test
  def equiparArmaduraEleganteSport(){
    val heroeConArmadura = superHeroe.equipar(armaduraEleganteSport)
    assertEquals(superHeroe.velocidadBase + 30, heroeConArmadura.stats().velocidad)
    assertEquals(superHeroe.hpBase - 30, heroeConArmadura.stats().hp)
  }
}