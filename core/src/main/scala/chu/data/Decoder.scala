package chu.data

import cats.arrow.{Choice, Arrow}
import cats.{CommutativeMonad, Contravariant, Monad, MonadError}
import cats.implicits.*
import chu.Decoder
import chu.data.Kleisli

/** Namespace for Decoder monad
  *
  * Computation type: Conversion attempt from an A type to a B type
  *
  * Encapsulation type: A => Either[E,B]
  * @tparam E
  *   error type
  * @tparam A
  *   input type
  * @tparam B
  *   output type
  */
trait Decoder[E, -A, B] extends Kleisli[Either[E, *], A, B]:
  override def local[C](f: C => A): Decoder[E, C, B] =
    Decoder.instance(run.compose(f))

  def contramap[C](f: C => A): Decoder[E, C, B] =
    local(f)

  def map[C](f: B => C): Decoder[E, A, C] =
    Decoder.instance(run(_).map(f))

  def emap[C](f: B => Either[E, C]): Decoder[E, A, C] =
    Decoder.instance(run(_).flatMap(f))

  def dimap[C, D](f: C => A)(g: B => D): Decoder[E, C, D] =
    Decoder.instance(c => run(f(c)).map(g))

  def lmap[C](f: C => A): Decoder[E, C, B] =
    contramap(f)

  def rmap[C](f: B => C): Decoder[E, A, C] =
    map(f)

  def flatMap[C, AA <: A](f: B => Decoder[E, AA, C]): Decoder[E, AA, C] =
    Decoder.instance(a => run(a).flatMap(f(_).run(a)))

  def andThen[C](f: B => Either[E, C]): Decoder[E, A, C] =
    Decoder.instance(run(_).flatMap(f))

  def andThen[C](fa: Decoder[E, B, C]): Decoder[E, A, C] =
    andThen(fa.run)

  def >>>[C](fa: Decoder[E, B, C]): Decoder[E, A, C] =
    andThen(fa.run)

  def compose[C, AA <: A](f: C => Either[E, AA]): Decoder[E, C, B] =
    Decoder.instance(f(_).flatMap(run))

  def compose[C, AA <: A](fa: Decoder[E, C, AA]): Decoder[E, C, B] =
    compose(fa.run)

  def <<<[C, AA <: A](fa: Decoder[E, C, AA]): Decoder[E, C, B] =
    compose(fa.run)

  def first[C]: Decoder[E, (A, C), (B, C)] =
    Decoder.instance { (a, c) => run(a).tupleRight(c) }

  def second[C]: Decoder[E, (C, A), (C, B)] =
    Decoder.instance { (c, a) => run(a).tupleLeft(c) }

object Decoder extends DecoderInstances:
  def apply[E, A, B](using Decoder[E, A, B]): Decoder[E, A, B] =
    summon[Decoder[E, A, B]]

  def instance[E, A, B](f: A => Either[E, B]): Decoder[E, A, B] =
    a => f(a)

  def pure[E, A, B](b: B): Decoder[E, A, B] =
    instance(_ => b.asRight)

  def raiseError[E, A, B](e: E): Decoder[E, A, B] =
    instance(_ => e.asLeft)

  def ask[E, A]: Decoder[E, A, A] =
    instance(a => a.asRight)

sealed abstract class DecoderInstances extends DecoderInstances0:
  given [A, E]: MonadError[Decoder[E, A, *], E] =
    new MonadError[Decoder[E, A, *], E] {
      def pure[B](b: B): Decoder[E, A, B] =
        Decoder.pure(b)

      def flatMap[B, C](fa: Decoder[E, A, B])(f: B => Decoder[E, A, C]): Decoder[E, A, C] =
        fa.flatMap(f)

      def tailRecM[B, C](b: B)(f: B => Decoder[E, A, Either[B, C]]): Decoder[E, A, C] =
        Decoder.instance { a =>
          Monad[Either[E, *]].tailRecM(b)(f(_).run(a))
        }

      def raiseError[B](e: E): Decoder[E, A, B] =
        Decoder.raiseError(e)

      def handleErrorWith[B](fb: Decoder[E, A, B])(f: E => Decoder[E, A, B]): Decoder[E, A, B] =
        Decoder.instance { (a: A) =>
          fb.run(a).leftFlatMap((e: E) => f(e).run(a))
        }
    }

  given [E]: Arrow[Decoder[E, *, *]] =
    new Arrow[Decoder[E, *, *]]:
      def lift[A, B](f: A => B): Decoder[E, A, B] =
        Decoder.instance(a => f(a).asRight)

      def first[A, B, C](fa: Decoder[E, A, B]): Decoder[E, (A, C), (B, C)] =
        fa.first

      def compose[A, B, C](f: Decoder[E, B, C], g: Decoder[E, A, B]): Decoder[E, A, C] =
        f.compose(g)

  given [E, B]: Contravariant[Decoder[E, *, B]] =
    new Contravariant[Decoder[E, *, B]]:
      def contramap[AA, A](fa: Decoder[E, AA, B])(f: A => AA): Decoder[E, A, B] =
        fa.local(f)

sealed abstract class DecoderInstances0:
  given [E]: Choice[Decoder[E, *, *]] =
    new Choice[Decoder[E, *, *]]:
      def id[A]: Decoder[E, A, A] =
        Decoder.ask[E, A]

      def choice[A, B, C](f: Decoder[E, A, C], g: Decoder[E, B, C]): Decoder[E, Either[A, B], C] =
        Decoder.instance(_.fold(f.run, g.run))

      def compose[A, B, C](f: Decoder[E, B, C], g: Decoder[E, A, B]): Decoder[E, A, C] =
        f.compose(g)
