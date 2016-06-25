package domain

import org.junit.Before
import org.junit.Test
import org.junit.Assert._


class MisionTest {
  
  val heroeNormal = Heroe(Stats(10,20,30,40))
  val heroeDebil = Heroe(Stats(1,2,1,4))
  val heroeFuerte = Heroe(Stats(100,100,100,100))

  val cascoVikingo = Item(Cabeza,10,efectos.modificarHP(+10),condiciones.fuerzaBaseMayorA(30))
  
  val tarea = Tarea(((_,_) => 10),((a,b)=>b))
  val tareaAfecta = Tarea(((_,_) => 10),((a,b)=>b.reemplazarMiembro(a,heroeDebil)))
  val tareaDificil = Tarea(((_,_) => 10),((a,b)=>b),(_ => false))
  val tareaFacilidad = Tarea(((h,e) => h.fuerzaBase + e.heroes.length ),((a,b)=>b))
  val pelearContraMonstruo = Tarea(tareas.facilidadMonstruo,tareas.efectoMonstruo(10))
  val peleaPorUnCasco = Tarea(((heroe,equipo) => heroe.fuerzaBase),((heroe,equipo)=> equipo.reemplazarMiembro(heroe, heroeFuerte.equipar(cascoVikingo))),(equipo => equipo.heroes.length > 2))
  val forzarPuerta = Tarea(tareas.facilidadPuerta,tareas.efectoForzarPuerta)
  val robarTalisman = Tarea(tareas.facilidadTalisman,tareas.efectoRobarTalisman(Item(Talisman,10,efectos.todosLosStatsEn(1))),tareas.condicionTalisman)
  
  val equipo = Equipo("Equipo lleno",0,List(heroeNormal))
  val equipoDeDos = equipo.obtenerMiembro(heroeDebil)
  val equipoDeTres = equipoDeDos.obtenerMiembro(heroeFuerte)
  
  val misionDe1 = Mision(List(tareaAfecta),(e => e))
  val misionImposible = Mision(List(tareaAfecta,tareaDificil),(e => e))
  
  val descripcionNoCumpleCondicion = "El equipo no cumplio la condicion"
  val descripcionNadiePuede = "Nadie puede realizarla"
  
  // tareas
  @Test
  def equipoRealizaTareaSinEfecto() = {
	  val e2 = tarea.serRealizadaPorTest(equipo)
			  assertEquals(equipo,e2)
  }

  @Test
  def equipoRealizaTareayafectaheroe() = {
	  val e2 = tareaAfecta.serRealizadaPorTest(equipo)
			  assertTrue(e2.heroes.contains(heroeDebil))
			  assertEquals(e2.heroes.length,1)
			  assertFalse(e2.heroes.contains(heroeNormal))
  }
  
  @Test
  def equipoFallaAlRealizarTareaSiNoCumpleCondicion() = {
	  val resultado = robarTalisman.serRealizadaPor(Exitosa(equipoDeTres,null))
	  assertEquals(Fallida(equipoDeTres,robarTalisman,descripcionNoCumpleCondicion),resultado)
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
		  pelearContraMonstruo.serRealizadaPorTest(equipo)
	  } catch {
	  case t: Throwable => assertTrue(t.getMessage == "no la realizo")
	  }
  }

  @Test
  def equipoPeleaYelHeroeGanaUnCasco() = {
	  val resultado = peleaPorUnCasco.serRealizadaPorTest(equipoDeTres)
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
	  val (e,_) = equipo.realizar(misionDe1)
			  assertTrue(e.heroes.contains(heroeDebil))
			  assertEquals(e.heroes.length,1)
			  assertFalse(e.heroes.contains(heroeNormal))
  }


  @Test
  def fracasanMisionImposible() = {
	  val (e,_) = equipo.realizar(misionImposible)
			  assertFalse(e.heroes.contains(heroeDebil))
			  assertEquals(e.heroes.length,1)
			  assertEquals(equipo,e)
			  assertTrue(e.heroes.contains(heroeNormal))
  }

}