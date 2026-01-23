package knot.data

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
  pureTest("map") {
    val fa = Encoder
      .instance[Int, String](_.toString)
      .map(s => s.head)
    expect.same(fa.run(12), '1')
  }
  pureTest("dimap") {
    val fa = Encoder
      .instance[Int, String](_.toString)
      .dimap[Long, Char](_.toInt)(s => s.head)
    expect.same(fa.run(12L), '1')
  }
  pureTest("lmap") {
    val fa = Encoder
      .instance[Int, String](_.toString)
      .lmap[Long](_.toInt)
    expect.same(fa.run(12L), "12")
  }
  pureTest("rmap") {
    val fa = Encoder
      .instance[Int, String](_.toString)
      .rmap(_.head)
    expect.same(fa.run(12), '1')
  }
  pureTest("andThen") {
    val fa = Encoder.instance[Int, String](_.toString)
    val fb = Encoder.instance[String, Char](_.head)
    val fc = fa.andThen(fb.run)
    val fd = fa.andThen(fb)
    val fe = fa >>> fb
    expect.same(fc.run(12), '1') and
      expect.same(fd.run(12), '1') and
      expect.same(fe.run(12), '1')
  }
  pureTest("compose") {
    val fa = Encoder.instance[String, Char](_.head)
    val fb = Encoder.instance[Int, String](_.toString)
    val fc = fa <<< fb
    expect.same(fc.run(12), '1')
  }
  pureTest("second") {
    val fa = Encoder.instance[String, Char](_.head).second[Int]
    expect.same(fa.run(1 -> "abc"), 1 -> 'a')
  }
}
