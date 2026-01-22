package chu.data

import cats.Eq
import cats.laws.discipline.*
import cats.laws.discipline.eq.*
import cats.laws.discipline.arbitrary.*
import org.scalacheck.{Arbitrary, Cogen, Gen}
import weaver.SimpleIOSuite
import weaver.discipline.Discipline

object EncoderSuite extends SimpleIOSuite with Discipline {
  given [A, B](using Eq[A => B]): Eq[Encoder[A, B]] =
    Eq.by[Encoder[A, B], A => B](_.run)

  given [A, B](using Arbitrary[A], Cogen[A], Arbitrary[B]): Arbitrary[Encoder[A, B]] =
    Arbitrary(Arbitrary.arbitrary[A => B].map(Encoder.instance))

  checkAll("Encoder[MiniInt, *]", CommutativeMonadTests[Encoder[MiniInt, *]].commutativeMonad[Int, Int, Int])
  checkAll("Encoder[*, *]", CommutativeArrowTests[Encoder[*, *]].commutativeArrow[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, Boolean])
  checkAll("Encoder[*, *]", ChoiceTests[Encoder[*, *]].choice[MiniInt, Boolean, Int, Int])
  checkAll("Encoder[*, Int]", ContravariantTests[Encoder[*, Int]].contravariant[MiniInt, Int, Boolean])
}
