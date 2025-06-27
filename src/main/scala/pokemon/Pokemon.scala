package pokemon

object Pokemon {
  trait Tipo {
    def debilidades: List[Tipo]
    def fortalezas: List[Tipo]
  }
  case object Agua extends Tipo {
    override def debilidades: List[Tipo] = List(Fuego)
    override def fortalezas: List[Tipo] = List(Fuego)
  }
  case object Fuego extends Tipo {
    override def debilidades: List[Tipo] = List(Agua)
    override def fortalezas: List[Tipo] = List()
  }
  case object Electrico extends Tipo {
    override def debilidades: List[Tipo] = List()
    override def fortalezas: List[Tipo] = List()
  }
  case object Fantasma extends Tipo {
    override def debilidades: List[Tipo] = List(Fantasma)
    override def fortalezas: List[Tipo] = List(Dragon)
  }
  val Dragon : Tipo = new Tipo {
    def debilidades: List[Tipo] = List(Fuego)
    lazy val fortalezas: List[Tipo] = List(Dragon)
  }

  case class Pokemon(especie : Especie,
                     caracteristicas: Caracteristicas, experiencia: Int = 0) {
    def energia : Int = this.caracteristicas.energia
    def energia(nuevaEnergia : Int) : Pokemon =
      this.copy(caracteristicas =
        this.caracteristicas.copy(energia = nuevaEnergia))

    def descansar : Pokemon = this.energia(this.caracteristicas.energiaMaxima)

  }

  case class Especie(nombre: String, tipo: Tipo, tipoSecundario: Option[Tipo] = None) {

  }
  val Pikachu = Especie("Pikachu", Electrico)
  val Ghastly = Especie("Ghastly", Dragon, Option(Fuego))

  case class Caracteristicas(
                              fuerza: Int,
                              velocidad: Int,
                              energiaMaxima: Int,
                              energia : Int
                            ){
    require(energia <= energiaMaxima, "La energia no puede ser mayor a la maxima")
    require(energia >= 0, "La energia no puede ser negativa")
    require(fuerza >= 0, "La fuerza no puede ser negativa")
    require(fuerza <= 100, "La fuerza no puede ser mayor a 100")
    require(velocidad >= 0, "La velocidad no puede ser negativa")
    require(velocidad <= 100, "La velocidad no puede ser mayor a 100")
  }

  type Actividad = Pokemon => Pokemon

  def descansar(pokemon: Pokemon): Pokemon =
    pokemon.energia(pokemon.caracteristicas.energiaMaxima)

  object Descansar extends Actividad{
    def apply(pokemon: Pokemon): Pokemon =
      pokemon.energia(pokemon.caracteristicas.energiaMaxima)
  }

  val actividad1 : Actividad = descansar(_)
  val actividad2 : Actividad = Descansar // permite garantizar que algo siempre ocurra, generalizando en Actividad
  val actividad3 : Actividad = _.descansar // enriqueciendo (o ensuciando) la interfaz del pokemon

  val rutina = List(actividad1, actividad2, actividad3)
}