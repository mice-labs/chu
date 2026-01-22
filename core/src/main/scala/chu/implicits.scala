package chu

import cats.implicits.*
import chu.data.{Decoder, Encoder}
import chu.text.Show
import chu.text.{Read, Show}
import fs2.Stream

import scala.util.Try

object implicits:
  extension [A](a: A)

    def show(using Show[A]): String =
      Show[A].run(a)
    def decode[E, B](using Decoder[E, A, B]): Either[E, B] =
      Decoder[E, A, B].run(a)
    def encode[B](using Encoder[A, B]): B =
      Encoder[A, B].run(a)
    def pickle[F[_]](using Pickle[F, A]): Stream[F, Byte] =
      Pickle[F, A].run(a)
    def serialize[F[_], B](using Serializer[F, A, B]): Stream[F, B] =
      Serializer[F, A, B].run(a)

  extension [F[_]](bytes: Stream[F, Byte])
    def unpickle[A](using Unpickle[F, A]): F[A] =
      Unpickle[F, A].run(bytes)

  extension (s: String)
    def read[E, A](using Read[E, A]): Either[E, A] =
      Read[E, A].run(s)
