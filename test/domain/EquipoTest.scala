package domain

import org.junit.Before
import org.junit.Test
import org.junit.Assert._

class EquipoTest {
  
  val lider = Heroe(Stats(50,50,50,50)).asignarTrabajo(Guerrero)
  val mago = Heroe(Stats(30,20,30,15)).asignarTrabajo(Mago)
  val ladron = Heroe(Stats(11,22,23,52)).asignarTrabajo(Ladron)
  val desocupado = Heroe(Stats(16,8,4,2))
  
  val equipoVacio = Equipo("Equipo vacío",0,List())
  val equipo = Equipo("Equipo lleno",0,List(lider,mago,ladron))
  
  @Before
  def setup() = {
     
  }

  
  //Mejor héroe según
  @Test
  def mejorHeroeSegunHPBase() = {
    assertEquals(Some(lider), equipo.mejorHeroeSegun { _.hpBase })
  }
  
  @Test
  def mejorHeroeSegunNoDevuelveNadaAnteEquipoVacio() = {
    assertEquals(None, equipoVacio.mejorHeroeSegun { _.hpBase })
  }
  
  //Manipular equipo
  @Test
  def agregarMiembroAEquipo() = {
    val cantidadMiembrosActual = equipo.cantidadMiembros
    assertEquals(cantidadMiembrosActual+1, equipo.obtenerMiembro(desocupado).cantidadMiembros)
  }
  
  @Test
  def reemplazarMiembroDeEquipo() = {
    val equipoModificado = equipo.reemplazarMiembro(ladron, desocupado)
    assertEquals(equipo.cantidadMiembros, equipoModificado.cantidadMiembros)
    assertTrue(equipoModificado.tieneA(desocupado))
    assertFalse(equipoModificado.tieneA(ladron))
  }
  
  @Test
  def reemplazarUnMiembroQueNoEstaSoloAgregaAlNuevo() = {
    val equipoModificado = equipoVacio.reemplazarMiembro(lider, mago)
    assertEquals(equipoVacio.cantidadMiembros+1, equipoModificado.cantidadMiembros)
    assertTrue(equipoModificado.tieneA(mago))
    assertFalse(equipoModificado.tieneA(lider))
  }

  
  //Lider
  @Test
  def liderDeEquipo() = {
    assertEquals(Some(lider), equipo.lider)
  }
  
  @Test
  def equipoVacioNoTieneLider() = {
    assertEquals(None, equipoVacio.lider)
  }
  
  @Test
  def siHayMasDeUnLiderNingunoLoEs() = {
    val otroLider = Heroe(Stats(18,22,64,45)).asignarTrabajo(Mago)
    val equipoModificado = equipo.obtenerMiembro(otroLider)
    assertEquals(None, equipoModificado.lider)
  }
  
  
  
}