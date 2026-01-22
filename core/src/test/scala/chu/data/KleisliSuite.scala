package chu.data

import cats.{Id, Eq, Eval}
import cats.laws.discipline.*
import cats.laws.discipline.eq.*
import cats.laws.discipline.arbitrary.*
import org.scalacheck.{Arbitrary, Cogen, Gen}
import weaver.SimpleIOSuite
import weaver.discipline.Discipline

object KleisliSuite extends SimpleIOSuite with Discipline {
  given [F[_], A, B](using Eq[A => F[B]]): Eq[Kleisli[F, A, B]] =
    Eq.by[Kleisli[F, A, B], A => F[B]](_.run)

  given [F[_], A, B](using Arbitrary[A], Cogen[A], Arbitrary[F[B]]): Arbitrary[Kleisli[F, A, B]] =
    Arbitrary(Arbitrary.arbitrary[A => F[B]].map(Kleisli.instance))

  checkAll("Kleisli[Either[MiniInt, *], MinInt, *]", ApplicativeErrorTests[Kleisli[Either[MiniInt, *], MiniInt, *], MiniInt].applicativeError[Int, Int, Int])
  checkAll("Kleisli[Eval, MiniInt, *]", DeferTests[Kleisli[Eval, MiniInt, *]].defer[Int])
  checkAll("Kleisli[Option, MiniInt, *]", MonadErrorTests[Kleisli[Either[MiniInt, *], MiniInt, *], MiniInt].monadError[Int, Int, Int])
  checkAll("Kleisli[Option, MiniInt, *]", SemigroupalTests[Kleisli[Option, MiniInt, *]].semigroupal[Int, Int, Int])
  checkAll("Kleisli[Option, MiniInt, *]", CommutativeFlatMapTests[Kleisli[Option, MiniInt, *]].commutativeFlatMap[Int, Int, Int])
  checkAll("Kleisli[Option, MiniInt, *]", CommutativeMonadTests[Kleisli[Option, MiniInt, *]].commutativeMonad[Int, Int, Int])
  checkAll("Kleisli[Id, MiniInt, *]", CommutativeMonadTests[Kleisli[Id, MiniInt, *]].commutativeMonad[Int, Int, Int])
  checkAll("Kleisli[List, *, *]", ArrowTests[Kleisli[List, *, *]].arrow[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, Boolean])
  checkAll("Kleisli[List, *, *]", ArrowChoiceTests[Kleisli[List, *, *]].arrowChoice[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, Boolean])
  checkAll("Kleisli[Option, *, *]", CommutativeArrowTests[Kleisli[Option, *, *]].commutativeArrow[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, Boolean])
  checkAll("Kleisli[Option, *, *]", ChoiceTests[Kleisli[Option, *, *]].choice[MiniInt, Boolean, Int, Int])
  checkAll("Kleisli[Option, MiniInt, *]", MonadTests[Kleisli[Option, MiniInt, *]].monad[Int, Int, Int])
  checkAll("Kleisli[Option, *, *]", StrongTests[Kleisli[Option, *, *]].strong[MiniInt, Boolean, Boolean, Boolean, Boolean, Int])
  checkAll("Kleisli[Option, MiniInt, Int]", FlatMapTests[Kleisli[Option, MiniInt, *]].flatMap[Int, Int, Int])
  checkAll("Kleisli[Option, MiniInt, *]", AlternativeTests[Kleisli[Option, MiniInt, *]].alternative[Int, Int, Int])
  checkAll("Kleisli[Option, *, Int]", ContravariantTests[Kleisli[Option, *, Int]].contravariant[MiniInt, Int, Boolean])
  checkAll("Kleisli[Option, MiniInt, Int]", ApplicativeTests[Kleisli[Option, MiniInt, *]].applicative[Int, Int, Int])
  checkAll("Kleisli[Option, MiniInt, Int]", ApplyTests[Kleisli[Option, MiniInt, *]].apply[Int, Int, Int])
  checkAll("Kleisli[Option, MiniInt, Int]", FunctorTests[Kleisli[Option, MiniInt, *]].functor[Int, Int, Int])
}
