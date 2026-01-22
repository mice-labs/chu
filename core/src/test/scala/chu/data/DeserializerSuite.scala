package chu.data

import cats.*
import cats.implicits.*
import cats.laws.discipline.*
import cats.laws.discipline.eq.*
import cats.laws.discipline.arbitrary.*
import cats.effect.IO
import org.scalacheck.Arbitrary
import weaver.SimpleIOSuite
import weaver.discipline.Discipline
import fs2.Stream
import chu.util.Fs2Instances.given
import chu.util.CatsEffectInstances.given

object DeserializerSuite extends SimpleIOSuite with Discipline {
  given [F[_], A, B](using Eq[Stream[F, A] => F[B]]): Eq[Deserializer[F, A, B]] =
    Eq.by[Deserializer[F, A, B], Stream[F, A] => F[B]](_.run)

  given [F[_], A, B](using Arbitrary[Stream[F, A] => F[B]]): Arbitrary[Deserializer[F, A, B]] =
    Arbitrary(Arbitrary.arbitrary[Stream[F, A] => F[B]].map(Deserializer.instance))

  checkAll("Deserializer[IO, MiniInt, *]", ApplicativeErrorTests[Deserializer[IO, MiniInt, *], Throwable].applicativeError[Int, Int, Int])
  checkAll("Deserializer[IO, MiniInt, *]", MonadErrorTests[Deserializer[IO, MiniInt, *], Throwable].monadError[Int, Int, Int])
  checkAll("Deserializer[Id, MiniInt, *]", SemigroupalTests[Deserializer[Id, MiniInt, *]].semigroupal[Int, Int, Int])
  checkAll("Deserializer[Id, MiniInt, *]", SemigroupalTests[Deserializer[Id, MiniInt, *]].semigroupal[Int, Int, Int])
  checkAll("Deserializer[Id, MiniInt, *]", MonadTests[Deserializer[Id, MiniInt, *]].monad[Int, Int, Int])
  checkAll("Deserializer[Id, MiniInt, Int]", FlatMapTests[Deserializer[Id, MiniInt, *]].flatMap[Int, Int, Int])
  checkAll("Deserializer[Id, *, Int]", ContravariantTests[Deserializer[Id, *, Int]].contravariant[MiniInt, Int, Boolean])
  pureTest("contramap") {
    val fa = Deserializer
      .instance[Id, Int, Int](s => s.compile.fold(0)(_ + _))
      .contramap[Boolean](b => if (b) 1 else 0)
    expect.same(fa.run(Stream(true, false, true)), 2)
  }
  pureTest("dimap") {
    val fa = Deserializer
      .instance[Id, Int, Int](s => s.compile.fold(0)(_ + _))
      .dimap[Boolean, String](b => if (b) 1 else 0)(_.toString)
    expect.same(fa.run(Stream(true, false, true)), "2")
  }
  pureTest("lmap") {
    val fa = Deserializer
      .instance[Id, Int, Int](s => s.compile.fold(0)(_ + _))
      .lmap[Boolean](b => if (b) 1 else 0)
    expect.same(fa.run(Stream(true, false, true)), 2)
  }
  pureTest("rmap") {
    val fa = Deserializer
      .instance[Id, Int, Int](s => s.compile.fold(0)(_ + _))
      .rmap(_.toString)
    expect.same(fa.run(Stream(1, 2, 3)), "6")
  }
  pureTest("unit") {
    val fa = Deserializer.unit[Id, Int]
    expect.same(fa.run(Stream(1)), ())
  }
  pureTest("liftF") {
    val fa = Deserializer.liftF[Id, String, Int](1)
    expect.same(fa.run(Stream("3")), 1)
  }

  object ImplicitResolution:
    Monoid[Deserializer[Option, MiniInt, Int]]
    Monad[Deserializer[Option, MiniInt, *]]
    MonoidK[Deserializer[List, MiniInt, *]]
    Alternative[Deserializer[Option, MiniInt, *]]
    CommutativeFlatMap[Deserializer[Option, MiniInt, *]]
    Semigroup[Deserializer[Option, MiniInt, Int]]
    SemigroupK[Deserializer[Option, MiniInt, *]]
    FlatMap[Deserializer[Option, MiniInt, *]]
    ApplicativeError[Deserializer[Either[Throwable, *], MiniInt, *], Throwable]
    Apply[Deserializer[Option, MiniInt, *]]
    Functor[Deserializer[Option, MiniInt, *]]
}
