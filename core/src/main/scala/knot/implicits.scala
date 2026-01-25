package knot

import knot.data.{Decoder, Encoder}
import knot.text.{Read, Show}

object implicits:
  extension [A](a: A)

    def show(using Show[A]): String =
      Show[A].run(a)
    def decode[E, B](using Decoder[E, A, B]): Either[E, B] =
      Decoder[E, A, B].run(a)
    def encode[B](using Encoder[A, B]): B =
      Encoder[A, B].run(a)

  extension (s: String)
    def read[E, A](using Read[E, A]): Either[E, A] =
      Read[E, A].run(s)
