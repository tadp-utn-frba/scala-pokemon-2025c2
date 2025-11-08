package pokemon.GimnasioPokemon

object GimnasioPokemon {
  case class Pokemon(experiencia: Int,
                     energia: Int,
                     stats: Stats,
                     especie: Especie,
                     estado: Estado = Sano
                    ) {
    require(energia <= energiaMaxima && energia >= 0)

    // accesors de stats
    lazy val energiaMaxima = this.stats.energiaMaxima
    lazy val velocidad = this.stats.velocidad
    lazy val fuerza = this.stats.fuerza

    def esDebil(otroTipo: Tipo): Boolean = ???

    // lazy val: se evalua por primera y unica vez cuando se lo llama.
    lazy val nivel = {
      //experiencia para llegar al nivel actual
      def nivelR(experienciaParaNivel: Int,
                 nivel: Int): Int = {
        val experienciaParaProximoNivel =
          2 * experienciaParaNivel + especie.resistenciaEvolutiva
        if (experienciaParaProximoNivel > experiencia) {
          nivel
        } else {
          nivelR(experienciaParaProximoNivel, nivel + 1)
        }
      }
      //Llamada recursiva de obtener el nivel a partir de la experiencia actual.
      nivelR(0, 1)
    }

    def aumentarStats: Pokemon = {
      this.copy(stats = this.stats + especie.aumentoStats)
    }

    def sePoneTriste: Pokemon = pierdeEnergia(10)

    //def descansar: Pokemon = ???

    def aumentarVelocidad(velocidadGanada: Int): Pokemon =
      this.copy(stats = stats.aumentarVelocidad(velocidadGanada))

    // asumimos que todas las actividades pasan por este metodo.
    def hacerActividad(actividad: Actividad): Pokemon = {
      if this.estado == KO then
        throw new Exception("El pokemon esta K.O.")
      else
        actividad(this)
    }

    def ganarExperiencia(xpGanada: Int) = {
      val pokemonNuevo = copy(experiencia= experiencia+xpGanada)
      if(pokemonNuevo.nivel > this.nivel) {
        // Chequear si sube de nivel y aumentar Stats
        val pokemonAumentado = pokemonNuevo.aumentarStats
        pokemonAumentado.especie.condicionEvolutiva.fold(pokemonAumentado)(
          ce => ce.intentarEvolucionarPorNivel(pokemonAumentado)
        )
      } else pokemonNuevo
    }

    def evolucionar(evolucion: Especie) = copy(especie=evolucion)

    def cambiaEstado(estadoNuevo: Estado) = copy(estado = estadoNuevo)

    def recuperarEnergiaMaxima = copy(energia = energiaMaxima)

    def pierdeEnergia(energiaPerdida: Int) = {
      copy(energia = (this.energia - energiaPerdida).max(0))
    }

    def recuperarEnergia(energiaARecuperar: Int) = {
      copy(energia = (this.energia + energiaARecuperar).min(this.energiaMaxima))
    }
  }
  // companion object
  object Pokemon {
  }

  type Actividad = Pokemon => Pokemon
  //trait ActividadT extends (Pokemon => Pokemon) {
  //}
  case object Descansar extends Actividad {
    override def apply(p: Pokemon): Pokemon =
      p.recuperarEnergiaMaxima
  }
  val descansar: Actividad = _.recuperarEnergiaMaxima

  case class LevantarPesas(kilos: Int) extends Actividad {
    override def apply(p: Pokemon): Pokemon = p match {
      case Fantasma(_,_) => throw new Exception("Un fantasma no puede levantar pesas")
      case _ if kilos >= (p.fuerza * 10) => p.pierdeEnergia(10)
      case _ => p.ganarExperiencia(experienciaAGanar(p))
    }

    def experienciaAGanar(p: Pokemon): Int = {
      if p.especie.esTipo(Peleador) then
        kilos * 2
      else
        kilos
    }
  }

  //Por cada minuto de nado, un Pokémon pierde 1
  // punto de Energía y gana 200 de Experiencia.
  //Los Pokémon de Tipo Agua ganan, además,
  // 1 punto de Velocidad por hora.
  //Los Pokémon con un Tipo Principal o Secundario que "pierde" contra el Tipo
  // Agua no ganan nada de
  //Experiencia y quedan K.O. automáticamente.
  // Los tipos que pierden contra agua son Roca, Tierra y
  //Fuego.
  case class Nadar(minutos: Int) extends Actividad {
    override def apply(p: Pokemon): Pokemon = {
      val pokemonEntrenado = p
        .pierdeEnergia(minutos)
        .ganarExperiencia(200*minutos)
      if p.esDebil(Agua) then
        p.cambiaEstado(KO) else
      if p.especie.esTipo(Agua) then
        pokemonEntrenado.aumentarVelocidad(minutos/60)
      else pokemonEntrenado
    }
  }

  case object Intercambiar extends Actividad {
    override def apply(p: Pokemon): Pokemon = {
      p.especie.condicionEvolutiva.fold(p.sePoneTriste)(
        ce => ce.intentarEvolucionarPorIntercambio(p)
      )
    }
  }

  trait Piedra {
    def afectaPokemon(pokemon: Pokemon): Boolean = ???
  }
  case class UsarPiedra(piedra: Piedra) extends Actividad {
    override def apply(p: Pokemon): Pokemon = {
      p.especie.condicionEvolutiva.fold(p)(
        ce => ce.intentarEvolucionarPorPiedra(p, piedra)
      )
    }
  }
  //def hacerActividad(pokemon: Pokemon,
  //                   actividad: ActividadT): Pokemon = actividad match {
  //    case Descansar => descansar(pokemon)
  //    case _ => ???
  //  }

  case class Stats(fuerza: Int,
                   velocidad: Int,
                   energiaMaxima: Int) {
    assert(fuerza > 0 && fuerza <= 100)
    assert(velocidad > 0 && velocidad <= 100)

    def aumentarVelocidad(velocidadNueva: Int): Stats =
      copy(velocidad = (velocidad + velocidadNueva).min(100))

    def +(otroStats: Stats): Stats = {
      copy(energiaMaxima = energiaMaxima + otroStats.energiaMaxima,
        fuerza = fuerza + otroStats.fuerza,
        velocidad = velocidad + otroStats.velocidad
      )
    }

    def *(otroStats: Stats): Stats = {
      copy(energiaMaxima = energiaMaxima * otroStats.energiaMaxima,
        fuerza = fuerza * otroStats.fuerza,
        velocidad = velocidad * otroStats.velocidad
      )
    }
  }

  trait CondicionEvolutiva(evolucion: Especie) {
    def intentarEvolucionarPorNivel(pokemon: Pokemon): Pokemon = pokemon
    def intentarEvolucionarPorIntercambio(pokemon: Pokemon): Pokemon = {
      pokemon.sePoneTriste
    }
    def intentarEvolucionarPorPiedra(pokemon: Pokemon, piedra: Piedra) = pokemon
  }

  class Nivel(evolucion: Especie, nivelMinimo: Int)
    extends CondicionEvolutiva(evolucion: Especie) {
    override def intentarEvolucionarPorNivel(pokemon: Pokemon): Pokemon = {
      if pokemon.nivel >= nivelMinimo then
        pokemon.evolucionar(evolucion)
      else
        pokemon
    }
  }

  class Intercambiar(evolucion: Especie)
    extends CondicionEvolutiva(evolucion: Especie) {
    override def intentarEvolucionarPorIntercambio(pokemon: Pokemon) =
      pokemon.evolucionar(evolucion)
  }

  class EvolucionPorPiedra(evolucion: Especie, piedraEvolucion: Piedra)
    extends CondicionEvolutiva(evolucion: Especie) {
    override def intentarEvolucionarPorPiedra(pokemon: Pokemon, piedra: Piedra) = {
      if piedraEvolucion == piedra then
        pokemon.evolucionar(evolucion)
      else pokemon
    }
  }

  case class Especie(tipoPrimario: Tipo,
                     tipoSecundario: Option[Tipo],
                     aumentoStats: Stats,
                     resistenciaEvolutiva: Int,
                     condicionEvolutiva: Option[CondicionEvolutiva]) {

    def esTipo(tipo: Tipo): Boolean =
      esTipoPrimario(tipo) || esTipoSecundario(tipo)
    def esTipoPrimario(tipo: Tipo): Boolean =
      tipoPrimario == tipo
    def esTipoSecundario(tipo: Tipo): Boolean =
      tipoSecundario.exists(_ == tipo)
  }

  // sealed previene que se pueda extender en otro package
  sealed trait Tipo {
    def unapply(pokemon: Pokemon): Option[(Boolean, Boolean)] = {
      val especie = pokemon.especie
      if especie.esTipo(this) then
        Some(especie.esTipoPrimario(this),
          especie.esTipoSecundario(this))
      else None
    }

    def esDebil(otroTipo: Tipo): Boolean = ???
  }
  case object Roca extends Tipo
  case object Agua extends Tipo
  case object Peleador extends Tipo
  case object Fantasma extends Tipo
  case object Fuego extends Tipo
  case object Electrico extends Tipo

  sealed trait Estado
  case object KO extends Estado
  case object Paralizado extends Estado
  case object Sano extends Estado
  case class Dormido(actividades_restantes: Int) extends Estado

  val pikachu = Pokemon(0, 100, Stats(100,
    20, 30), Especie(Electrico, None, Stats(10, 1,1), 2, None))


  val pikachuEntrenado = pikachu.hacerActividad(descansar)
  //val pikachuEntrenado2 = pikachu.hacerActividad(
  //  hacerActividad(_, Descansar)
  //)
  //val pikachuEntrenado3 = pikachu.hacerActividad(
  //  _.descansar
  //)
  //val fxDescansar: Actividad = _.descansar
  val fxDescansar2: Actividad = Descansar
  val pikachuEntrenado4 = pikachu.hacerActividad(
    Descansar
  )
  Nadar(2).apply(pikachuEntrenado4)
}








