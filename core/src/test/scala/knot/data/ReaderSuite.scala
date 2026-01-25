package knot.data

import cats.Eq
import cats.laws.discipline.*
import cats.laws.discipline.eq.*
import cats.laws.discipline.arbitrary.*
import org.scalacheck.{Arbitrary, Cogen}
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
  pureTest("Reader[Int, String]: map") {
    val fa = Reader
      .instance[Int, String](_.toString)
      .map(s => s.head)
    expect.eql(fa.run(12), '1')
  }
  pureTest("Reader[Int, String]: dimap") {
    val fa = Reader
      .instance[Int, String](_.toString)
      .dimap[Long, Char](_.toInt)(s => s.head)
    expect.eql(fa.run(12L), '1')
  }
  pureTest("Reader[Int, String]: lmap") {
    val fa = Reader
      .instance[Int, String](_.toString)
      .lmap[Long](_.toInt)
    expect.eql(fa.run(12L), "12")
  }
  pureTest("Reader[Int, String]: rmap") {
    val fa = Reader
      .instance[Int, String](_.toString)
      .rmap(_.head)
    expect.eql(fa.run(12), '1')
  }
  pureTest("Reader[Int, String]: andThen") {
    val fa = Reader.instance[Int, String](_.toString)
    val fb = Reader.instance[String, Char](_.head)
    val fc = fa.andThen(fb.run)
    val fd = fa.andThen(fb)
    val fe = fa >>> fb
    expect.eql(fc.run(12), '1') and
      expect.eql(fd.run(12), '1') and
      expect.eql(fe.run(12), '1')
  }
  pureTest("Reader[String, Char]: compose") {
    val fa = Reader.instance[String, Char](_.head)
    val fb = Reader.instance[Int, String](_.toString)
    val fc = fa <<< fb
    expect.eql(fc.run(12), '1')
  }
  pureTest("Reader[String, Char]: second") {
    val fa = Reader.instance[String, Char](_.head).second[Int]
    expect.eql(fa.run(1 -> "abc"), 1 -> 'a')
  }
  object ImplicitResolution:
    given Reader[Int, String] =
      Reader.instance(_.toString)
    Reader[Int, String]
}
