package chu.data

import cats.{Eq, Id}
import cats.effect.IO
import cats.laws.discipline.*
import cats.laws.discipline.eq.*
import cats.laws.discipline.arbitrary.*
import fs2.Stream
import chu.util.Fs2Instances.given
import chu.util.CatsEffectInstances.given
import org.scalacheck.{Arbitrary, Cogen, Gen}
import weaver.SimpleIOSuite
import weaver.discipline.Discipline

object UnpickleSuite extends SimpleIOSuite with Discipline {
  given ExhaustiveCheck[Byte] =
    ExhaustiveCheck.instance((Byte.MinValue to Byte.MaxValue).map(_.toByte).toList)

  given [F[_], A](using Eq[Stream[F, Byte] => F[A]]): Eq[Unpickle[F, A]] =
    Eq.by[Unpickle[F, A], Stream[F, Byte] => F[A]](_.run)

  given [F[_], A](using Cogen[Stream[F, Byte]], Arbitrary[F[A]]): Arbitrary[Unpickle[F, A]] =
    Arbitrary(Arbitrary.arbitrary[Stream[F, Byte] => F[A]].map(Unpickle.instance))

  checkAll("Unpickle[IO, MiniInt, *]", ApplicativeErrorTests[Unpickle[IO, *], Throwable].applicativeError[Int, Int, Int])
}
