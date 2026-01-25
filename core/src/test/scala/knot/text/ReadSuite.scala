package knot.text

import cats.Eq
import cats.implicits.*
import cats.laws.discipline.*
import cats.laws.discipline.eq.*
import cats.laws.discipline.arbitrary.*
import org.scalacheck.{Arbitrary, Cogen}
import weaver.SimpleIOSuite
import weaver.discipline.Discipline

object ReadSuite extends SimpleIOSuite with Discipline {
  given ExhaustiveCheck[String] =
    ExhaustiveCheck.instance(List.tabulate(10)("abcdefghij".take))
  given [E, A](using Eq[String => Either[E, A]]): Eq[Read[E, A]] =
    Eq.by[Read[E, A], String => Either[E, A]](_.run)

  given [E, A](using Arbitrary[E], Arbitrary[A]): Arbitrary[Read[E, A]] =
    Arbitrary(Arbitrary.arbitrary[String => Either[E, A]].map(Read.instance))

  checkAll("Read[MinInt, *]", ApplicativeErrorTests[Read[MiniInt, *], MiniInt].applicativeError[Int, Int, Int])
  pureTest("Read[String, Int]: map") {
    val fa = Read
      .instance[String, Int](s => Either.catchNonFatal(s.toInt).leftMap(_.getMessage))
      .map(_ + 1)
    expect.eql(fa.run("10"), 11.asRight) and
      expect(fa.run("abc").isLeft)
  }

  object ImplicitResolution:
    given Read[Throwable, Int] = Read
      .instance[Throwable, Int](s => Either.catchNonFatal(s.toInt))
    Read[Throwable, Int]
}
