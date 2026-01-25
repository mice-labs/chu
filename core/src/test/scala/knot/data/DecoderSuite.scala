package knot.data

import cats.Eq
import cats.implicits.*
import cats.laws.discipline.*
import cats.laws.discipline.eq.*
import cats.laws.discipline.arbitrary.*
import org.scalacheck.{Arbitrary, Cogen}
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

  pureTest("Decoder[String, String, Int]: contramap") {
    val fa = Decoder
      .instance[String, String, Int](s => Either.catchNonFatal(s.toInt).leftMap(_.getMessage))
      .contramap[Long](l => l.toString)
    expect.eql(fa.run(123L), 123.asRight) and
      expect(fa.run(Long.MaxValue).isLeft)
  }
  pureTest("Decoder[String, String, Int]: map") {
    val fa = Decoder
      .instance[String, String, Int](s => Either.catchNonFatal(s.toInt).leftMap(_.getMessage))
      .map(_ + 1)
    expect.eql(fa.run("10"), 11.asRight) and
      expect(fa.run("abc").isLeft)
  }
  pureTest("Decoder[String, String, Int]: dimap") {
    val fa = Decoder
      .instance[String, String, Int](s => Either.catchNonFatal(s.toInt).leftMap(_.getMessage))
      .dimap[Long, Int](l => l.toString)(_ + 1)
    expect.eql(fa.run(124L), 125.asRight) and
      expect(fa.run(Long.MaxValue).isLeft)
  }
  pureTest("Decoder[String, String, Int]: lmap") {
    val fa = Decoder
      .instance[String, String, Int](s => Either.catchNonFatal(s.toInt).leftMap(_.getMessage))
      .lmap[Long](l => l.toString)
    expect.eql(fa.run(123L), 123.asRight) and
      expect(fa.run(Long.MaxValue).isLeft)
  }
  pureTest("Decoder[String, String, Int]: rmap") {
    val fa = Decoder
      .instance[String, String, Int](s => Either.catchNonFatal(s.toInt).leftMap(_.getMessage))
      .rmap(_ + 1)
    expect.eql(fa.run("10"), 11.asRight) and
      expect(fa.run("abc").isLeft)
  }
  pureTest("Decoder[String, String, Int]: andThen") {
    val fa = Decoder
      .instance[String, String, Int](s => Either.catchNonFatal(s.toInt).leftMap(_.getMessage))
    val fb = Decoder
      .instance[String, Int, Int](i => Either.catchNonFatal(10 / i).leftMap(_.getMessage))
    val fc = fa.andThen(fb.run)
    val fd = fa.andThen(fb)
    val fe = fa >>> fb
    expect.eql(fc.run("10"), 1.asRight) and
      expect(fc.run("abc").isLeft) and
      expect(fc.run("0").isLeft) and
      expect.eql(fd.run("10"), 1.asRight) and
      expect(fd.run("abc").isLeft) and
      expect(fd.run("0").isLeft) and
      expect.eql(fe.run("10"), 1.asRight) and
      expect(fe.run("abc").isLeft) and
      expect(fe.run("0").isLeft)
  }
  pureTest("Decoder[String, Int, Int]: compose") {
    val fa = Decoder
      .instance[String, Int, Int](i => Either.catchNonFatal(10 / i).leftMap(_.getMessage))
    val fb = Decoder
      .instance[String, String, Int](s => Either.catchNonFatal(s.toInt).leftMap(_.getMessage))
    val fc = fa <<< fb
    expect.eql(fc.run("10"), 1.asRight) and
      expect(fc.run("abc").isLeft) and
      expect(fc.run("0").isLeft)
  }
  pureTest("Decoder[String, String, Int]: second") {
    val fa = Decoder
      .instance[String, String, Int](s => Either.catchNonFatal(s.toInt).leftMap(_.getMessage))
      .second[String]
    expect.eql(fa.run("10" -> "10"), ("10" -> 10).asRight) and
      expect(fa.run("1" -> "abc").isLeft)
  }

  object ImplicitResolution:
    given Decoder[String, String, Int] = Decoder.instance(s => Either.catchNonFatal(s.toInt).leftMap(_.getMessage))
    Decoder[String, String, Int]
}
