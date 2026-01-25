package knot.data

import cats.arrow.FunctionK
import cats.data.EitherNel
import cats.{Eq, Eval, Id}
import cats.implicits.*
import cats.laws.discipline.*
import cats.laws.discipline.eq.*
import cats.kernel.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import org.scalacheck.{Arbitrary, Cogen}
import weaver.SimpleIOSuite
import weaver.discipline.Discipline

import scala.util.Try

object KleisliSuite extends SimpleIOSuite with Discipline {
  given [F[_], A, B](using Eq[A => F[B]]): Eq[Kleisli[F, A, B]] =
    Eq.by[Kleisli[F, A, B], A => F[B]](_.run)

  given [F[_], A, B](using Arbitrary[A], Cogen[A], Arbitrary[F[B]]): Arbitrary[Kleisli[F, A, B]] =
    Arbitrary(Arbitrary.arbitrary[A => F[B]].map(Kleisli.instance))

  checkAll("Kleisli[Id, MiniInt, *]", CommutativeMonadTests[Kleisli[Id, MiniInt, *]].commutativeMonad[Int, Int, Int])
  checkAll("Kleisli[Id, MiniInt, *]", CommutativeArrowTests[Kleisli[Id, *, *]].commutativeArrow[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, Boolean])
  checkAll("Kleisli[Eval, MiniInt, *]", DeferTests[Kleisli[Eval, MiniInt, *]].defer[Int])
  checkAll("Kleisli[List, MiniInt, Int]", FunctorFilterTests[Kleisli[List, MiniInt, *]].functorFilter[Int, Int, Int])
  checkAll("Kleisli[Option, *, *]", CommutativeArrowTests[Kleisli[Option, *, *]].commutativeArrow[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, Boolean])
  checkAll("Kleisli[Option, MiniInt, *]", CommutativeMonadTests[Kleisli[Option, MiniInt, *]].commutativeMonad[Int, Int, Int])
  checkAll("Kleisli[Option, MiniInt, Int]", MonoidTests[Kleisli[Option, MiniInt, Int]].monoid)
  checkAll("Kleisli[Option, MiniInt, *]", MonadErrorTests[Kleisli[Either[MiniInt, *], MiniInt, *], MiniInt].monadError[Int, Int, Int])
  checkAll("Kleisli[List, *, *]", ArrowChoiceTests[Kleisli[List, *, *]].arrowChoice[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, Boolean])
  checkAll("Kleisli[Option, MiniInt, *]", MonadTests[Kleisli[Option, MiniInt, *]](Kleisli.monadForKleisli).monad[Int, Int, Int])
  checkAll("Kleisli[EitherNel[MiniInt, *], MiniInt, *]", ParallelTests[Kleisli[EitherNel[MiniInt, *], MiniInt, *]].parallel[Int, Int])
  checkAll("Kleisli[Option, *, Int]", ContravariantTests[Kleisli[Option, *, Int]].contravariant[MiniInt, Int, Boolean])
  checkAll("Kleisli[Option, MiniInt, *]", AlternativeTests[Kleisli[Option, MiniInt, *]](Kleisli.alternativeForKleisli).alternative[Int, Int, Int])
  checkAll("Kleisli[List, MiniInt, *]", MonoidKTests[Kleisli[List, MiniInt, *]](Kleisli.monoidKForKleisli).monoidK[Int])
  checkAll("Kleisli[Option, MiniInt, Int]", CommutativeFlatMapTests[Kleisli[Option, MiniInt, *]](Kleisli.commutativeFlatMapForKleisli).flatMap[Int, Int, Int])
  checkAll("Kleisli[Id, *, *]", ChoiceTests[Kleisli[Id, *, *]](Kleisli.choiceForKleisli).choice[MiniInt, Boolean, Int, Int])
  checkAll("Kleisli[Option, *, *]", ComposeTests[Kleisli[Option, *, *]](Kleisli.composeForKleisli).compose[MiniInt, Boolean, Int, Int])
  checkAll("Kleisli[Option, *, *]", StrongTests[Kleisli[Option, *, *]](Kleisli.strongForKleisli).strong[MiniInt, Boolean, Boolean, Boolean, Boolean, Int])
  checkAll("Kleisli[Option, MiniInt, Int]", SemigroupTests[Kleisli[Option, MiniInt, Int]](Kleisli.semigroupForKleisli).semigroup)
  checkAll("Kleisli[Option, *, *]", SemigroupKTests[Kleisli[Option, MiniInt, *]](Kleisli.semigroupKForKleisli).semigroupK[MiniInt])
  checkAll("Kleisli[Option, MiniInt, Int]", FlatMapTests[Kleisli[Option, MiniInt, *]](Kleisli.flatMapForKleisli).flatMap[Int, Int, Int])
  checkAll(
    "Kleisli[Either[MiniInt, *], MinInt, *]",
    ApplicativeErrorTests[Kleisli[Either[MiniInt, *], MiniInt, *], MiniInt](Kleisli.applicativeErrorForKleisli).applicativeError[Int, Int, Int]
  )
  checkAll("Kleisli[Option, MiniInt, Int]", ApplicativeTests[Kleisli[Option, MiniInt, *]](Kleisli.applicativeForKleisli).applicative[Int, Int, Int])
  checkAll("Kleisli[Option, MiniInt, Int]", ApplyTests[Kleisli[Option, MiniInt, *]](Kleisli.applyForKleisli).apply[Int, Int, Int])
  checkAll("Kleisli[Option, MiniInt, Int]", FunctorTests[Kleisli[Option, MiniInt, *]](Kleisli.functorForKleisli).functor[Int, Int, Int])
  pureTest("Kleisli[Option, String, Int]: mapFilter") {
    val fa = Kleisli
      .instance[Option, String, Int](s => Try(s.toInt).toOption)
      .mapFilter[Int](i => if (i % 2 == 0) Some(i) else None)
    expect.eql(fa.run("12"), 12.some) and
      expect.eql(fa.run("abc"), None) and
      expect.eql(fa.run("9"), None)
  }
  pureTest("Kleisli[List, Int, String]: mapK") {
    val fa = Kleisli
      .instance[List, Int, String](i => List(i.toString))
      .mapK(FunctionK.id)
    expect.eql(fa.run(12), List("12"))
  }
  pureTest("Kleisli[Id, Int, String]: lmap") {
    val fa = Kleisli
      .instance[Id, Int, String](_.toString)
      .lmap[Long](_.toInt)
    expect.eql(fa.run(12L), "12")
  }
  pureTest("Kleisli[Id, Int, String]: rmap") {
    val fa = Kleisli
      .instance[Id, Int, String](_.toString)
      .rmap(_.head)
    expect.eql(fa.run(12), '1')
  }
  pureTest("Kleisli[Id, String, Char]: compose") {
    val fa = Kleisli.instance[Id, String, Char](_.head)
    val fb = Kleisli.instance[Id, Int, String](_.toString)
    val fc = fa <<< fb
    expect.eql(fc.run(12), '1')
  }
  pureTest("Kleisli: shift") {
    val fa = Kleisli.shift[Map[String, *], Double, Double](d => Map(d.toString -> d))
    expect.eql(fa.run(2.2d), Map("2.2" -> 2.2d))
  }
  pureTest("Kleisli: choose") {
    val fa = Kleisli.instance[Id, Double, String](_.toString)
    val fb = Kleisli.instance[Id, Int, String](_.toString)
    val fc = Kleisli.choose(fa)(fb)
    expect.eql(fc.run(2.asRight), "2".asRight) and
      expect.eql(fc.run(0.2d.asLeft), "0.2".asLeft)
  }
  pureTest("Kleisli: choice") {
    val fa = Kleisli.instance[Id, Double, String](_.toString)
    val fb = Kleisli.instance[Id, Int, String](_.toString)
    val fc = Kleisli.choice(fa, fb)
    expect.eql(fc.run(2.asRight), "2") and
      expect.eql(fc.run(0.2d.asLeft), "0.2")
  }
}
