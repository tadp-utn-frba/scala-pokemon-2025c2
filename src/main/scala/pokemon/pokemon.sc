trait Tipo
case object Roca extends Tipo
case object Agua extends Tipo
case object Peleador extends Tipo
case object Fantasma extends Tipo
case object Fuego extends Tipo
case object Electrico extends Tipo

val l = List(Roca, Agua, Peleador)
val l2  = l.collect(
  {
    case Roca => Agua
    case Agua => Electrico
  }
)
print(l2)
object B {
  def apply(): Int = 2
}

object A {
  val m = B
}

print(A.m)