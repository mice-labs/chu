package knot.data

import cats.Eq
import cats.implicits.*
import cats.laws.discipline.*
import cats.laws.discipline.eq.*
import cats.laws.discipline.arbitrary.*
import org.scalacheck.{Arbitrary, Cogen, Gen}
import weaver.{Expectations, SimpleIOSuite}
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

  pureTest("contramap") {
    val fa = Decoder
      .instance[Throwable, String, Int](s => Either.catchNonFatal(s.toInt))
      .contramap[Long](l => l.toString)
    expect.same(fa.run(123L), 123.asRight) and
      expect(fa.run(Long.MaxValue).isLeft)
  }
  pureTest("map") {
    val fa = Decoder
      .instance[Throwable, String, Int](s => Either.catchNonFatal(s.toInt))
      .map(_ + 1)
    expect.same(fa.run("10"), 11.asRight) and
      expect(fa.run("abc").isLeft)
  }
  pureTest("dimap") {
    val fa = Decoder
      .instance[Throwable, String, Int](s => Either.catchNonFatal(s.toInt))
      .dimap[Long, Int](l => l.toString)(_ + 1)
    expect.same(fa.run(124L), 125.asRight) and
      expect(fa.run(Long.MaxValue).isLeft)
  }
  pureTest("lmap") {
    val fa = Decoder
      .instance[Throwable, String, Int](s => Either.catchNonFatal(s.toInt))
      .lmap[Long](l => l.toString)
    expect.same(fa.run(123L), 123.asRight) and
      expect(fa.run(Long.MaxValue).isLeft)
  }
  pureTest("rmap") {
    val fa = Decoder
      .instance[Throwable, String, Int](s => Either.catchNonFatal(s.toInt))
      .rmap(_ + 1)
    expect.same(fa.run("10"), 11.asRight) and
      expect(fa.run("abc").isLeft)
  }
  pureTest("andThen") {
    val fa = Decoder
      .instance[Throwable, String, Int](s => Either.catchNonFatal(s.toInt))
    val fb = Decoder
      .instance[Throwable, Int, Int](i => Either.catchNonFatal(10 / i))
    val fc = fa.andThen(fb.run)
    val fd = fa.andThen(fb)
    val fe = fa >>> fb
    expect.same(fc.run("10"), 1.asRight) and
      expect(fc.run("abc").isLeft) and
      expect(fc.run("0").isLeft) and
      expect.same(fd.run("10"), 1.asRight) and
      expect(fd.run("abc").isLeft) and
      expect(fd.run("0").isLeft) and
      expect.same(fe.run("10"), 1.asRight) and
      expect(fe.run("abc").isLeft) and
      expect(fe.run("0").isLeft)
  }
  pureTest("compose") {
    val fa = Decoder
      .instance[Throwable, Int, Int](i => Either.catchNonFatal(10 / i))
    val fb = Decoder
      .instance[Throwable, String, Int](s => Either.catchNonFatal(s.toInt))
    val fc = fa <<< fb
    expect.same(fc.run("10"), 1.asRight) and
      expect(fc.run("abc").isLeft) and
      expect(fc.run("0").isLeft)
  }
  pureTest("second") {
    val fa = Decoder
      .instance[Throwable, String, Int](s => Either.catchNonFatal(s.toInt))
      .second[String]
    expect.same(fa.run("10" -> "10"), ("10" -> 10).asRight) and
      expect(fa.run("1" -> "abc").isLeft)
  }

  object ImplicitResolution:
    given Decoder[Throwable, String, Int] = Decoder.instance(s => Either.catchNonFatal(s.toInt))
    Decoder[Throwable, String, Int]
}
