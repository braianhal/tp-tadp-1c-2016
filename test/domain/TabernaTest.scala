package domain

import org.junit.Before
import org.junit.Test
import org.junit.Assert._


class TabernaTest {
  
  val lider = Heroe(Stats(50,50,50,50)).asignarTrabajo(Guerrero)
  val mago = Heroe(Stats(30,20,30,15)).asignarTrabajo(Mago)
  val ladron = Heroe(Stats(11,22,23,52)).asignarTrabajo(Ladron)
  val desocupado = Heroe(Stats(16,8,4,2))
  
  val criterioOro:((Equipo,Equipo) => Boolean) = { (e1,e2) => e1.pozo > e2.pozo }
  
  val equipoVacio = Equipo("Equipo vacío",0,List())
  val superEquipo = Equipo("Equipo lleno",100,List(lider,mago,ladron))
  
  val tarea = Tarea(((_,_) => 10),((_,equipo)=>equipo.copy(pozo = 1000)))
  val tareaImposible = Tarea(((_,_) => 10),((a,b)=>b),(_ => false))
  
  val misionNula = Mision(List(),(equipo => equipo))
  val misionSimple = Mision(List(tarea),(equipo => equipo))
  val misionImposible = Mision(List(tarea,tareaImposible),(e => e))
  
  val taberna = Taberna(List(misionNula,misionSimple))
  val tabernaDeMoe = Taberna(List(misionImposible,misionImposible,misionImposible))
  val tabernaVacia = Taberna(List())
  
  // ELEGIR MISION
  @Test
   def equipoConMasOroGana() = {
	  assertTrue(criterioOro(superEquipo,equipoVacio))
  }
  
  @Test
   def entreDosMisionesExitosasSeEligeLaMejor() = {
    val unaMision = (misionNula,misionNula.serRealizadaPor(superEquipo))
    val otraMision = (misionSimple,misionSimple.serRealizadaPor(superEquipo))
	  assertEquals(otraMision,taberna.elMejor(criterioOro)(unaMision,otraMision))
  }
  
  @Test
   def entreUnaExitosaYUnaFallidaSeEligeLaExitosa() = {
    val misionExitosa = (misionNula,misionNula.serRealizadaPor(superEquipo))
    val misionFallida = (misionImposible,misionImposible.serRealizadaPor(superEquipo))
	  assertEquals(misionExitosa,taberna.elMejor(criterioOro)(misionExitosa,misionFallida))
  }
  
  @Test
   def eligeUnaDeLasFallidasSiAmbasLoSon() = {
    val misionFallida = (misionImposible,misionImposible.serRealizadaPor(superEquipo))
    val misionFallida2 = misionFallida.copy()
	  assertEquals(misionFallida,taberna.elMejor(criterioOro)(misionFallida,misionFallida2))
  }
  
  @Test
   def siLaMisionElegidaNoEsExitosaNoSeDevuelve() = {
    val misionFallida = (misionImposible,misionImposible.serRealizadaPor(superEquipo))
	  assertEquals(None,taberna.exitosaONinguna(misionFallida))
  }
  
  @Test
   def siLaMisionElegidaEsExitosaSeDevuelve() = {
    val misionExitosa = (misionNula,misionNula.serRealizadaPor(superEquipo))
	  assertEquals(Some(misionNula),taberna.exitosaONinguna(misionExitosa))
  }
  
  @Test
   def elegirMisionNoDevuelveNadaSiTodasFallan() = {
	  assertEquals(None,tabernaDeMoe.elegirMision(superEquipo, criterioOro))
  }
  
  @Test
   def elegirMisionNoDevuelveNadaSiNoHayNinguna() = {
	  assertEquals(None,tabernaVacia.elegirMision(superEquipo, criterioOro))
  }
  
  @Test
   def elegirMisionDevuelveLaMejorDeLasExitosas() = {
	  assertEquals(Some(misionSimple),taberna.elegirMision(superEquipo, criterioOro))
  }

}