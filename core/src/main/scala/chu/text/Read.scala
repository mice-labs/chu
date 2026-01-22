package chu.text

import cats.implicits.*
import cats.{Applicative, ApplicativeError, Apply}
import cats.evidence.As
import chu.data.{Decoder, Kleisli}

/** Namespace for Read Computation type: Conversion from a readable string to an A type Encapsulation type: String => Either[E, A]
  * @tparam E
  *   error type
  * @tparam A
  *   output type
  */
trait Read[E, A] extends Decoder[E, String, A]:
  override def map[B](f: A => B): Read[E, B] =
    Read.instance(run(_).map(f))

  override def emap[B](f: A => Either[E, B]): Read[E, B] =
    Read.instance(run(_).flatMap(f))

  def handleErrorWith(f: E => Read[E, A]): Read[E, A] =
    Read.instance(s => run(s).leftFlatMap(f(_).run(s)))

  def ap[B, C](ff: Read[E, B])(using ev: A As (B => C)): Read[E, C] =
    Read.instance(s => ff.run(s).ap(run(s).map(ev.coerce)))

object Read:
  def apply[E, A](using Read[E, A]): Read[E, A] =
    summon[Read[E, A]]

  def instance[E, A](f: String => Either[E, A]): Read[E, A] =
    s => f(s)

  def pure[E, A](a: A): Read[E, A] =
    instance(_ => a.asRight)

  def raiseError[E, A](e: E): Read[E, A] =
    instance(_ => e.asLeft)

  given [E]: ApplicativeError[Read[E, *], E] =
    new ApplicativeError[Read[E, *], E]:
      def pure[A](a: A): Read[E, A] = Read.pure(a)

      def ap[A, B](ff: Read[E, A => B])(fa: Read[E, A]): Read[E, B] =
        ff.ap(fa)

      def handleErrorWith[A](fa: Read[E, A])(f: E => Read[E, A]): Read[E, A] =
        fa.handleErrorWith(f)

      def raiseError[A](e: E): Read[E, A] =
        Read.raiseError(e)
