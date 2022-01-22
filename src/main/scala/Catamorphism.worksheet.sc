// helpers

trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
}

final case class Fix[F[_]](unFix: F[Fix[F]])

// catamorphism
def catamorphism[F[_], A](algebra: F[A] => A)(fa: Fix[F])(
    implicit F: Functor[F]
): A = algebra(F.map(fa.unFix)(catamorphism(algebra)))

// example

sealed trait Expr[+A]
final case class Value(i: Int) extends Expr[Nothing]
final case class Add[A](left: A, right: A) extends Expr[A]

implicit val functorExpr: Functor[Expr] = new Functor[Expr] {
    override def map[A, B](fa: Expr[A])(f: A => B): Expr[B] = 
        fa match {
            case v @ Value(_) => v
            case Add(left, right) => Add(f(left), f(right))
        }
}

val eval: Expr[Int] => Int = {
    case Value(i) => i
    case Add(left, right) => left + right
}

val prettyPrint: Expr[String] => String = {
    case Value(i) => i.toString
    case Add(left, right) => s"$left + $right"
}

val expr: Fix[Expr] = Fix(
    Add(
        Fix(
            Add(
                Fix(Value(1) : Expr[Fix[Expr]]),
                Fix(Value(2) : Expr[Fix[Expr]])
            )
        ),
        Fix(Value(3) : Expr[Fix[Expr]])
    )
)

catamorphism(eval)(expr) // 6
catamorphism(prettyPrint)(expr) // "1 + 2 + 3"
