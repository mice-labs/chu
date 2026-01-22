package chu.data

import cats.Eq
import cats.laws.discipline.*
import cats.laws.discipline.eq.*
import cats.laws.discipline.arbitrary.*
import org.scalacheck.{Arbitrary, Cogen, Gen}
import weaver.SimpleIOSuite
import weaver.discipline.Discipline

object ReaderSuite extends SimpleIOSuite with Discipline {
  given [A, B](using Eq[A => B]): Eq[Reader[A, B]] =
    Eq.by[Reader[A, B], A => B](_.run)

  given [A, B](using Arbitrary[A], Cogen[A], Arbitrary[B]): Arbitrary[Reader[A, B]] =
    Arbitrary(Arbitrary.arbitrary[A => B].map(Reader.instance))

  checkAll("Reader[MiniInt, *]", CommutativeMonadTests[Reader[MiniInt, *]].commutativeMonad[Int, Int, Int])
  checkAll("Reader[*, *]", CommutativeArrowTests[Reader[*, *]].commutativeArrow[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, Boolean])
  checkAll("Reader[*, *]", ChoiceTests[Reader[*, *]].choice[MiniInt, Boolean, Int, Int])
  checkAll("Reader[*, Int]", ContravariantTests[Reader[*, Int]].contravariant[MiniInt, Int, Boolean])

}
