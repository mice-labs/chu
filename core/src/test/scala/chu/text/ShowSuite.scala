package chu.text

import cats.Eq
import cats.laws.discipline.*
import cats.laws.discipline.eq.*
import cats.laws.discipline.arbitrary.*
import org.scalacheck.{Arbitrary, Cogen, Gen}
import weaver.SimpleIOSuite
import weaver.discipline.Discipline

object ShowSuite extends SimpleIOSuite with Discipline {
  given [A](using Eq[A => String]): Eq[Show[A]] =
    Eq.by[Show[A], A => String](_.run)

  given [A](using Arbitrary[A], Cogen[A]): Arbitrary[Show[A]] =
    Arbitrary(Arbitrary.arbitrary[A => String].map(Show.instance))

  checkAll("Show", ContravariantTests[Show].contravariant[MiniInt, Int, Boolean])
}
