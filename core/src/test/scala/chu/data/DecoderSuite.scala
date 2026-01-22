package chu.data

import cats.Eq
import cats.laws.discipline.*
import cats.laws.discipline.eq.*
import cats.laws.discipline.arbitrary.*
import org.scalacheck.{Arbitrary, Cogen, Gen}
import weaver.SimpleIOSuite
import weaver.discipline.Discipline

object DecoderSuite extends SimpleIOSuite with Discipline {
  given [E, A, B](using Eq[A => Either[E, B]]): Eq[Decoder[E, A, B]] =
    Eq.by[Decoder[E, A, B], A => Either[E, B]](_.run)

  given [E, A, B](using Arbitrary[A], Cogen[A], Arbitrary[E], Arbitrary[B]): Arbitrary[Decoder[E, A, B]] =
    Arbitrary(Arbitrary.arbitrary[A => Either[E, B]].map(Decoder.instance))

  checkAll("Decoder[MiniInt, MiniInt, *]", MonadErrorTests[Decoder[Int, MiniInt, *], Int].monadError[Int, Int, Int])
  checkAll("Decoder[MiniInt, *, *]", ArrowTests[Decoder[Int, *, *]].arrow[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, Boolean])
  checkAll("Decoder[MiniInt, *, *]", ChoiceTests[Decoder[Int, *, *]].choice[MiniInt, Boolean, Int, Int])
  checkAll("Decoder[MiniInt, *, Int]", ContravariantTests[Decoder[Int, *, Int]].contravariant[MiniInt, Int, Boolean])
}
