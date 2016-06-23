package domain

import org.junit.Before
import org.junit.Test
import org.junit.Assert._


class MisionTest {
  
  val heroeNormal = Heroe(Stats(10,20,30,40))
  val heroeDebil = Heroe(Stats(1,2,1,4))
  val heroeFuerte = Heroe(Stats(100,100,100,100))

  val cascoVikingo = Item(Cabeza,10,efectos.modificarHP(+10),condiciones.fuerzaBaseMayorA(30))
  
  val tarea = Tarea(((_,_) => 10),(_ => true),((a,b)=>b))
  val tareaAfecta = Tarea(((_,_) => 10),(_ => true),((a,b)=>b.reemplazarMiembro(a,heroeDebil)))
  val tareaDificil = Tarea(((_,_) => 10),(_ => false),((a,b)=>b))
  val tareaFacilidad = Tarea(((h,e) => h.fuerzaBase + e.heroes.length ),(_ => true),((a,b)=>b))
  val pelearContraMonstruo = Tarea(((heroe,equipo) => 10), (equipo => equipo.heroes.length > 1),((heroe,equipo)=> equipo.reemplazarMiembro(heroe, heroeFuerte)))
  val peleaPorUnCasco = Tarea(((heroe,equipo) => heroe.fuerzaBase), (equipo => equipo.heroes.length > 2),((heroe,equipo)=> equipo.reemplazarMiembro(heroe, heroeFuerte.equipar(cascoVikingo))))

  
  val equipo = Equipo("Equipo lleno",0,List(heroeNormal))
  val equipoDeDos = equipo.obtenerMiembro(heroeDebil)
  val equipoDeTres = equipoDeDos.obtenerMiembro(heroeFuerte)
  
  val misionDe1 = new Mision(List(tareaAfecta),(e => e))
  val misionImposible = new Mision(List(tareaAfecta,tareaDificil),(e => e))
  
 @Test
  def equipoRealizaTareaSinEfecto() = {
    val e2 = tarea.serRealizadaPor(equipo)
    assertEquals(equipo,e2)
  }
  
   @Test
  def equipoRealizaTareayafectaheroe() = {
    val e2 = tareaAfecta.serRealizadaPor(equipo)
    assertTrue(e2.heroes.contains(heroeDebil))
    assertEquals(e2.heroes.length,1)
    assertFalse(e2.heroes.contains(heroeNormal))
  }
   
  @Test
  def facilidadParaHeroe() = {
    assertEquals(tareaFacilidad.facilidad(heroeNormal,equipo),21)
  }
  
  @Test
  def seEligeHeroeConMayorFacilidadParaTarea() = {
    val heroeElegido = tareaFacilidad.mejorHeroeParaTarea(equipoDeDos)
    val heroeFinal = heroeElegido match {
      case Some(h) => h
      case _ => null
    }
    assertEquals(heroeFinal,heroeNormal)
  }
   
  @Test
  def equipoNoPuedeRealizarUnaTareaPorFaltaDeEquipo() = {
   val error =  try {
     pelearContraMonstruo.serRealizadaPor(equipo)
   } catch {
     case t: Throwable => assertTrue(t.getMessage == "no la realizo")
   }
   
    //val resultado = pelearContraMonstruo.serRealizadaPor(equipo)
    //assertEquals(resultado,equipo)
  }
   
  @Test
  def equipoPeleaYelHeroeSeReemplaza() = {
    val resultado = pelearContraMonstruo.serRealizadaPor(equipoDeDos)
    assertTrue(resultado.heroes.contains(heroeFuerte))
    assertEquals(resultado.heroes.length,2)    
  }
  
   @Test
  def equipoPeleaYelHeroeGanaUnCasco() = {
    val resultado = peleaPorUnCasco.serRealizadaPor(equipoDeTres)
    assertTrue(resultado.heroes.contains(heroeDebil))
    assertTrue(resultado.heroes.contains(heroeNormal))
    assertEquals(resultado.heroes.length,3)
    assertFalse(resultado.heroes.contains(heroeFuerte))
    val h = resultado.heroes.find { heroe => heroe.inventario.tiene(cascoVikingo) } 
    val heroeConCasco = h match {
      case Some(h) => h
      case _ => null
    }
    assertTrue(resultado.heroes.contains(heroeConCasco))
    assertTrue(heroeConCasco.inventario.tiene(cascoVikingo))
  }
  
  
  //---------misiones
   @Test
  def equipoRealizaMisionyafectaheroe() = {
    val e = misionDe1.realizarMision(equipo)
    assertTrue(e.heroes.contains(heroeDebil))
    assertEquals(e.heroes.length,1)
    assertFalse(e.heroes.contains(heroeNormal))
  }
   
  
   @Test
  def fracasanMisionImposible() = {
     val e = misionImposible.realizarMision(equipo)
    assertFalse(e.heroes.contains(heroeDebil))
    assertEquals(e.heroes.length,1)
    assertEquals(equipo,e)
    assertTrue(e.heroes.contains(heroeNormal))
  }
   
   
   
   
   
   
   
   
}