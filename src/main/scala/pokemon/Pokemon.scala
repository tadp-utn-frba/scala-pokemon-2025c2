package pokemon

import pokemon.Pokemon.TieneTipo

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

  case object Pelea extends Tipo {
    override def debilidades: List[Tipo] = List()

    override def fortalezas: List[Tipo] = List()
  }


  case object Fantasma extends Tipo {
    override def debilidades: List[Tipo] = List(Fantasma)

    override def fortalezas: List[Tipo] = List(Dragon)
  }

  val Dragon: Tipo = new Tipo {
    def debilidades: List[Tipo] = List(Fuego)

    lazy val fortalezas: List[Tipo] = List(Dragon)
  }

  /// Pokemon
  
  case class Pokemon(especie: Especie,
                     caracteristicas: Caracteristicas,
                     experiencia: Int = 0) {
    def energia: Int = this.caracteristicas.energia

    def energia(nuevaEnergia: Int): Pokemon =
      this.copy(caracteristicas =
        this.caracteristicas.copy(energia = nuevaEnergia))
    
    def descansar: Pokemon = this.energia(this.caracteristicas.energiaMaxima)

    def recuperaEnergiaMaxima = this.energia(this.caracteristicas.energiaMaxima)
    
    def pierdeEnergia(energiaPerdida: Int) = this.energia(this.energia - energiaPerdida)
  
    def ganarExperiencia(experienciaGanada: Int) = this.copy(experiencia = this.experiencia + experienciaGanada)
  
    def ganaVelocidad(velocidadGenada: Float) = ???
    
    lazy val nivel = ???
  }

  case class Especie(nombre: String,
                     tipo: Tipo,
                     tipoSecundario: Option[Tipo] = None,
                     condicionEvolutiva: Option[CondicionEvolutiva]) {

    def tieneTipo(tipo: Tipo): Boolean =
      tieneTipoPrimario(tipo) || tieneTipoSecundario(tipo)
  
    def tieneTipoPrimario(tipoP: Tipo): Boolean =
      tipoP == tipo
    def tieneTipoSecundario(tipoP: Tipo): Boolean =
      this.tipoSecundario.contains(tipoP)
  }

  val Pikachu = Especie("Pikachu", Electrico)
  val Ghastly = Especie("Ghastly", Dragon, Option(Fuego))

  case class Caracteristicas(
                              fuerza: Int,
                              velocidad: Int,
                              energiaMaxima: Int,
                              energia: Int
                            ) {
    require(energia <= energiaMaxima, "La energia no puede ser mayor a la maxima")
    require(energia >= 0, "La energia no puede ser negativa")
    require(fuerza >= 0, "La fuerza no puede ser negativa")
    require(fuerza <= 100, "La fuerza no puede ser mayor a 100")
    require(velocidad >= 0, "La velocidad no puede ser negativa")
    require(velocidad <= 100, "La velocidad no puede ser mayor a 100")
  }
  
  ///Evoluciones
  
  //Subir de Nivel: Los miembros de las Especies con esta condición evolucionan a otra especie cuando
  //alcanzan un cierto nivel, que puede variar para cada Especie.
  //
  // Intercambiar: Los miembros de Especies con esta condición sólo evolucionan como consecuencia del
  //profundo trauma emocional que sufren si creen que sus dueños los han "Intercambiado" por otro
  //Pokémon. Los Pokémon cuya Especie tiene como Condición Evolutiva "Intercambiar", evolucionan.
  //Los demás sólo se ponen tristes y pierden 10 de energía.
  //
  // Usar Piedra: Los miembros de Especies con esta condición sólo 
  // evolucionan cuando son expuestos a
  //la radiación de unos objetos conocidos como "Piedras Evolutivas". 
  // Cada Piedra tiene asociado un Tipo
  //que debe coincidir con el Tipo Principal de la Especie para 
  // gatillar la evolución. La única excepción a
  //esto son las Piedras Lunares, que hacen evolucionar Pokémon 
  // de especies arbitrarias. Por cada
  //especie, se sabe si evoluciona por una piedra lunar o no.
  
  abstract class CondicionEvolutiva() {
      def siguienteEspecie: Especie
      def usarPiedra(tipo: Tipo, pokemon: Pokemon): Pokemon = pokemon
  }
  
  class EvolucionConPiedra(tipoPiedra: Tipo, val siguienteEspecie: Especie) extends CondicionEvolutiva {
    def usarPiedra(tipo: Tipo, pokemon: Pokemon): Pokemon = {
      if (tipo == tipoPiedra) {
        pokemon.copy(especie = siguienteEspecie)
      } else {
        pokemon
      }
    }
  }

  /// Actividades
  
  type Actividad = Pokemon => Pokemon

  def descansar(pokemon: Pokemon): Pokemon = pokemon.recuperaEnergiaMaxima

  object Descansar extends Actividad{
    def apply(pokemon: Pokemon): Pokemon = pokemon.recuperaEnergiaMaxima
  }
  
  class TieneTipo(tipo: Tipo) {
    def unapply(pokemon: Pokemon): Option[Unit] = {
      if (pokemon.especie.tieneTipo(tipo)) {
        Some(Unit)
      }
      else {
        None
      }
    }
  }
  
  case class Nadar(minutos: Int) extends Actividad {
    //Por cada minuto de nado, un Pokémon pierde 1 punto de Energía y 
    //gana 200 de Experiencia.
    //Los Pokémon de Tipo Agua ganan, además, 1 punto de Velocidad por hora.
    def apply(pokemon: Pokemon): Pokemon = {
      val pokemonEntrenado = 
        pokemon.pierdeEnergia(minutos).ganarExperiencia(200*minutos)
      
      val tieneTipoAgua = TieneTipo(Agua)
      pokemonEntrenado match {
        case p if p.especie.tieneTipo(Agua) => ???
        case tieneTipoAgua => pokemon.ganaVelocidad(minutos/60)
        case _ => pokemon
      }
    }
  }
  
  case class LevantarPesas(kilos: Int) extends Actividad {
    // Cuando un Pokémon levanta pesas, 
    // gana 1 punto de experiencia por cada kilo levantado.
    // Si un Pokémon levanta más de 10 kilos por cada punto de Fuerza, 
    // no gana nada de Experiencia y pierde 10 de energía.
    //Los Pokémon de Tipo Pelea ganan el doble de puntos.
    //Los Pokémon de Tipo Fantasma NO PUEDEN levantar pesas (es decir, son incapaces de realizar la
    //actividad, sin importar el peso a levantar ni ningún otro factor).
    def apply(pokemon: Pokemon): Pokemon = {
      
      val tieneTipoFantasma = TieneTipo(Fantasma)
      val tieneTipoPelea = TieneTipo(Pelea)
      
      pokemon match {
        case tieneTipoFantasma => pokemon
        case p if kilos >= p.caracteristicas.fuerza * 10 => pokemon.pierdeEnergia(10)
        case tieneTipoPelea => pokemon.ganarExperiencia(kilos*2)
        case _ => pokemon.ganarExperiencia(kilos)
      }
    }
  }
  
  case class UsarPiedra(tipoPiedra: Tipo) extends Actividad {
    def apply(pokemon: Pokemon): Pokemon = {
      pokemon.especie.condicionEvolutiva.fold(pokemon)(
        c => c.usarPiedra(tipoPiedra, pokemon))
    }
  }
  
  case class Intercambiar() extends Actividad {
    def apply(pokemon: Pokemon): Pokemon = ???
  }

  val actividad2 : Actividad = Descansar // permite garantizar que algo siempre ocurra, generalizando en Actividad
  val actividad1 : Actividad = Nadar(10)
  val actividad3 : Actividad = LevantarPesas(10)
  val rutina = List(actividad1, actividad2)
  
}