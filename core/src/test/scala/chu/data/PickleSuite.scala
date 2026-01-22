package chu.data

import cats.{Id, Eq}
import cats.laws.discipline.*
import cats.laws.discipline.eq.*
import cats.laws.discipline.arbitrary.*
import fs2.Stream
import chu.util.Fs2Instances.given
import org.scalacheck.{Arbitrary, Cogen, Gen}
import weaver.SimpleIOSuite
import weaver.discipline.Discipline

object PickleSuite extends SimpleIOSuite with Discipline {
  given [F[_], A](using Eq[A => Stream[F, Byte]]): Eq[Pickle[F, A]] =
    Eq.by[Pickle[F, A], A => Stream[F, Byte]](_.run)

  given [F[_], A](using Arbitrary[A], Cogen[A]): Arbitrary[Pickle[F, A]] =
    Arbitrary(Arbitrary.arbitrary[A => Stream[F, Byte]].map(Pickle.instance))

  checkAll("Pickle[Id, *]", ContravariantTests[Pickle[Id, *]].contravariant[MiniInt, Int, Boolean])
}
