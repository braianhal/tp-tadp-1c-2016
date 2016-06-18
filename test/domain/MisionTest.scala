package domain

import org.junit.Before
import org.junit.Test
import org.junit.Assert._

class MisionTest {
  
  val heroe = Heroe(Stats(10,20,30,40))
  val heroe2 = Heroe(Stats(1,2,1,4))
  
  val tarea = Tarea(((_,_) => 10),(_ => true),((a,b)=>b))
  val tareaAfecta = Tarea(((_,_) => 10),(_ => true),((a,b)=>b.reemplazarMiembro(a,heroe2)))
  val tareaFacilidad = Tarea(((h,e) => h.fuerzaBase + e.heroes.length ),(_ => true),((a,b)=>b))
  val equipo = Equipo("Equipo lleno",0,List(heroe))
  val equipoDeDos = equipo.obtenerMiembro(heroe2)
    
  
  @Test
  def equipoRealizaTarea() = {
    val e2 = tarea.serRealizadaPor(equipo)
    assertEquals(equipo,e2)
  }
  
   @Test
  def equipoRealizaTareayafectaheroe() = {
    val e2 = tareaAfecta.serRealizadaPor(equipo)
    assertTrue(e2.heroes.contains(heroe2))
    assertEquals(e2.heroes.length,1)
    assertFalse(e2.heroes.contains(heroe))
  }
   
  @Test
  def facilidadParaHeroe() = {
    assertEquals(tareaFacilidad.facilidad(heroe,equipo),21)
  }
  
  @Test
  def seEligeHeroeConMayorFacilidadParaTarea() = {
    val heroeElegido = tareaFacilidad.mejorHeroeParaTarea(equipoDeDos)
    val heroeFinal = heroeElegido match {
      case Some(h) => h
      case _ => null
    }
    assertEquals(heroeFinal,heroe)
  }
   
   
   
   
   
   
   
   
   
   
   
}