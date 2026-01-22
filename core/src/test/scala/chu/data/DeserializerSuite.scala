package chu.data

import cats.{Eq, Id}
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
}
