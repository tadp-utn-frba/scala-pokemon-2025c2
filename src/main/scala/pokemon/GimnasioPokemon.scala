package pokemon.GimnasioPokemon

import pokemon.GimnasioPokemon.GimnasioPokemon.EsDeTipo

object GimnasioPokemon {
  case class Pokemon(experiencia: Int,
                     stats: Stats,
                     especie: Especie
                    ) {
    def energiaMaxima = stats.energiaMaxima

    def energiaMaxima_=(newEnergiaMaxima: Int) =
      this.copy(stats = this.stats.copy(energiaMaxima = newEnergiaMaxima))

    def descansar = Descansar()(this)

    def setStats(energia: Int = this.stats.energia,
                 energiaMaxima: Int = this.stats.energiaMaxima,
                 fuerza: Int = this.stats.fuerza,
                 velocidad: Int = this.stats.velocidad) =
      this.copy(stats = Stats(energia, energiaMaxima, fuerza, velocidad))
  }
  object Pokemon {
//    def unapply(p: Pokemon) = {
//      Option((p.experiencia, p.stats, p.especie))
//    }
  }

  case class Stats(energia: Int,
                   energiaMaxima: Int,
                   fuerza: Int,
                   velocidad: Int) {
    require(energia > 0 && energia <= energiaMaxima)
    require(fuerza > 1 && fuerza <= 100)
    require(velocidad > 1 && velocidad <= 100)

  }

  case class Especie(tipoPrincipal: Tipo, tipoSecundario: Option[Tipo]) {
      def esTipo(tipo: Tipo) =
        esTipoPrimario(tipo) || esTipoSecundario(tipo)

      def esTipoPrimario(tipo: Tipo) = tipoPrincipal == tipo
      def esTipoSecundario(tipo: Tipo) = tipoSecundario == tipo
  }


  trait Tipo {
    def unapply(pokemon: Pokemon): Option[Tipo] = {
      val especie = pokemon.especie
      Option(this).filter(especie.esTipoPrimario)
    }
  }
  case object Fantasma extends Tipo
  case object Fuego extends Tipo
  case object Agua extends Tipo
  case object Roca extends Tipo
  case object Pelea extends Tipo
  case object Electrico extends Tipo

  object EsDeTipo {
    def unapply(pokemon: Pokemon): Option[Tipo] =
      Option(pokemon.especie.tipoPrincipal)
  }

  object Tipos {
    def unapplySeq(pokemon: Pokemon): Option[Seq[Tipo]] = {
      val especie = pokemon.especie
      Option(especie.tipoPrincipal::especie.tipoSecundario.toList)
    }
  }

  type Actividad = Pokemon => Pokemon

  //definir actividad como trait
  // objetos como funciones
  trait Actividad2 {
    def apply(pokemon: Pokemon): Pokemon
  }

  class Descansar extends Actividad {
    override def apply(pokemon: Pokemon): Pokemon =
      pokemon.setStats(energia = pokemon.energiaMaxima)
  }

  def descansar(pokemon: Pokemon): Pokemon =
    pokemon.setStats(energia = pokemon.energiaMaxima)

  // sin efecto. Retorna otro pokemon.
  def levantarPesas(pokemon: Pokemon): Pokemon = {
    pokemon match {
      case Fantasma => ???
      case EsDeTipo(x) if x == Fantasma => ???
      case Tipos(Fantasma, Fuego) => ???
      case Tipos(xs:_*) if xs.contains(Fantasma) => ???
    }
  }













  val statsPikachu = Stats(energia = 100, energiaMaxima = 200, fuerza = 10, velocidad = 35)
  val pikachu = Pokemon.apply(
    experiencia= 100,
    stats= statsPikachu,
    especie = ???
  )


  descansar(pikachu)
  descansar.apply(pikachu)
  new Descansar().apply(pikachu)
  new Descansar()(pikachu)


  var actividad: Actividad = descansar
  actividad = descansar.apply(_)
  actividad = new Descansar()
  actividad = new Descansar()(_)
  actividad = _.descansar
  actividad =  p => p.descansar





  //val pikachuEntrenado = pikachu.hacerActividad(descansar)
  //val pikachuEntrenado2 = pikachu.hacerActividad(
  //  hacerActividad(_, Descansar)
  //)
  //val pikachuEntrenado3 = pikachu.hacerActividad(
  //  _.descansar
  //)
  //val fxDescansar: Actividad = _.descansar
  //  val fxDescansar2: Actividad = Descansar
  //  val pikachuEntrenado4 = pikachu.hacerActividad(
  //    Descansar
  //  )
  //  Nadar(2).apply(pikachuEntrenado4)
}
















// lazy val: se evalua por primera y unica vez cuando se lo llama.
//    lazy val nivel = {
//      //experiencia para llegar al nivel actual
//      def nivelR(experienciaParaNivel: Int,
//                 nivel: Int): Int = {
//        val experienciaParaProximoNivel =
//          2 * experienciaParaNivel + especie.resistenciaEvolutiva
//        if (experienciaParaProximoNivel > experiencia) {
//          nivel
//        } else {
//          nivelR(experienciaParaProximoNivel, nivel + 1)
//        }
//      }
//      //Llamada recursiva de obtener el nivel a partir de la experiencia actual.
//      nivelR(0, 1)
//    }


