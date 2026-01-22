package chu.data

import cats.{Id, Eq}
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

object SerializerSuite extends SimpleIOSuite with Discipline {
  given [F[_], A, B](using Eq[A => Stream[F, B]]): Eq[Serializer[F, A, B]] =
    Eq.by[Serializer[F, A, B], A => Stream[F, B]](_.run)

  given [F[_], A, B](using Arbitrary[A => Stream[F, B]]): Arbitrary[Serializer[F, A, B]] =
    Arbitrary(Arbitrary.arbitrary[A => Stream[F, B]].map(Serializer.instance))

  checkAll("Serializer[IO, MiniInt, *]", MonadErrorTests[Serializer[IO, MiniInt, *], Throwable].monadError[Int, Int, Int])
  checkAll("Serializer[Id, *, *]", ArrowTests[Serializer[Id, *, *]].arrow[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, Boolean])
  checkAll("Serializer[Id, *, *]", ChoiceTests[Serializer[Id, *, *]].choice[MiniInt, Boolean, Int, Int])
  checkAll("Serializer[Id, MiniInt, *]", MonadTests[Serializer[Id, MiniInt, *]].monad[Int, Int, Int])
  checkAll("Serializer[Id, *, Int]", ContravariantTests[Serializer[Id, *, Int]].contravariant[MiniInt, Int, Boolean])
}
