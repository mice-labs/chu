package knot

import implicits.*
import cats.implicits.*
import knot.text.{Read, Show}
import weaver.SimpleIOSuite

object ImplicitsSpec extends SimpleIOSuite:
  pureTest("Int: show") {
    given Show[Int] =
      Show.instance(_.toString)
    expect.eql(67.show, "67")
  }
  pureTest("String: decode") {
    given Decoder[String, String, Int] =
      Decoder.instance(s => Either.catchNonFatal(s.toInt).leftMap(_.getMessage))

    expect.eql("67".decode[String, Int], 67.asRight) and
      expect("abc".decode[String, Int].isLeft)
  }
  pureTest("Int: encode") {
    given Encoder[Int, String] =
      Encoder.instance(_.toString)

    expect.eql(67.encode[String], "67")
  }
  pureTest("String: read") {
    given Read[String, Int] =
      Read.instance(s => Either.catchNonFatal(s.toInt).leftMap(_.getMessage))

    expect.eql("67".read[String, Int], 67.asRight) and
      expect("abc".read[String, Int].isLeft)
  }
