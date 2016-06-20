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
  val otroEquipo = Equipo("Un Equipo",0,List(mago,ladron))
  
  val cascoCaro = Item(Cabeza,300,efectos.modificarHP(+10))
  val cascoDeFuerza = Item(Cabeza,300,efectos.modificarFuerza(+50))
  val superCasco = Item(Cabeza,1000,efectos.modificarTodos(+10))
  
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
  
  @Test
  def unEquipoDeUnSoloHeroeSeTieneASiMismoComoLider() = {
    val equipito = Equipo("Equipito",100,List(desocupado))
    assertEquals(Some(desocupado), equipito.lider)
  } 
  
  
  // Manejo de ítems
  @Test
  def venderItemAumentaElPozo() = {
    assertEquals(equipo.pozo + 300, equipo.vender(cascoCaro).pozo)
  }
  
  @Test
  def aHeroeLeSirveUnItemPorqueAumentaSuStatPrincipal() = {
    assertTrue(lider.leSirve(cascoDeFuerza))
  }
  
  @Test
  def aHeroeNoLeSirveItemQueNoAumenteSuStatPrincipal() = {
    assertFalse(mago.leSirve(cascoCaro))
  }
  
  @Test
  def aUnHeroeSinTrabajoNoLeSirveNingunItem() = {
    assertFalse(desocupado.leSirve(superCasco))
  }
  
  @Test
  def elBeneficioDeUnItemQueSirveEsPositivo() = {
    assertTrue(lider.beneficioDe(cascoDeFuerza) > 0)
  }
  
  @Test
  def elBeneficioDeUnItemQueNoSirveNoEsPositivo() = {
    assertFalse(mago.beneficioDe(cascoDeFuerza) > 0)
  }
  
  @Test
  def unHeroeSinTrabajoSiempreTieneBeneficio0() = {
    assertEquals(0,desocupado.beneficioDe(superCasco))
  }
  
  @Test
  def darleItemAUnMiembroProduceUnEfectoSobreElMismo() = {
    val fuerzaActual = mago.stats().fuerza
    
    val equipoMejorado = equipo.darleItemA(mago, cascoDeFuerza)
    val magoMejorado = equipoMejorado.obtenerLosQueSean(Mago).head
    assertEquals(fuerzaActual+50, magoMejorado.stats().fuerza)
  }
  
  @Test
  def heroeAlQueMasLeSirveElStatPrincipal() = {
    assertEquals(Some(lider), equipo.alQueMasLeSirve(cascoDeFuerza))
  }
  
  @Test
  def siElEquipoNoTieneHeroesANingunoLeSirve() = {
    assertEquals(None, equipoVacio.alQueMasLeSirve(cascoDeFuerza))
  }
  
  @Test
  def siANingunoLeAumentaElStatPrincipalANadieLeSirve() = {
    assertEquals(None, otroEquipo.alQueMasLeSirve(cascoDeFuerza))
  }
  
  @Test
  def alObtenerItemSeLoDaAlHeroeQueMasLeSirva() = {
    val equipoEquipado = equipo.obtenerItem(cascoDeFuerza)
    
    val liderEquipado = equipoEquipado.obtenerLosQueSean(Guerrero).head
    val magoSinEquipar = equipoEquipado.obtenerLosQueSean(Mago).head
    assertEquals(lider.stats().fuerza + 50, liderEquipado.stats().fuerza)
    assertEquals(mago.stats().fuerza, magoSinEquipar.stats().fuerza)
  }
  
  @Test
  def siANadieLeSirveUnItemSeVende() = {
    val equipoNuevo = otroEquipo.obtenerItem(cascoDeFuerza)
    
    assertEquals(mago, equipoNuevo.obtenerLosQueSean(Mago).head)
    assertEquals(ladron, equipoNuevo.obtenerLosQueSean(Ladron).head)
    assertEquals(otroEquipo.pozo + 300, equipoNuevo.pozo)
  }
  
  
}