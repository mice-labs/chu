package knot.data

import cats.arrow.{Choice, CommutativeArrow}
import cats.{CommutativeMonad, Contravariant, Id}
import knot.data.{Kleisli, Reader}

/** Namespace for Encoder monad
  *
  * Computation type: Conversion from an A type to a B type
  *
  * Encapsulation type: A => B
  * @tparam A
  *   input type
  * @tparam B
  *   output type
  */
trait Encoder[-A, B] extends Kleisli[Id, A, B]:
  override def local[C](f: C => A): Encoder[C, B] =
    Encoder.instance(run.compose(f))

  def map[C](f: B => C): Encoder[A, C] =
    Encoder.instance(run.andThen(f))

  def dimap[C, D](f: C => A)(g: B => D): Encoder[C, D] =
    Encoder.instance(f.andThen(run).andThen(g))

  def lmap[C](f: C => A): Encoder[C, B] =
    local(f)

  def rmap[C](f: B => C): Encoder[A, C] =
    map(f)

  def flatMap[C, AA <: A](f: B => Encoder[AA, C]): Encoder[AA, C] =
    Encoder.instance(a => f(run(a)).run(a))

  def andThen[C](f: B => C): Encoder[A, C] =
    map(f)

  def andThen[C](fa: Reader[B, C]): Encoder[A, C] =
    andThen(fa.run)

  def >>>[C](fa: Reader[B, C]): Encoder[A, C] =
    andThen(fa.run)

  def compose[C, AA <: A](f: C => AA): Encoder[C, B] =
    local(f)

  def compose[C, AA <: A](fa: Encoder[C, AA]): Encoder[C, B] =
    compose(fa.run)

  def <<<[C, AA <: A](fa: Encoder[C, AA]): Encoder[C, B] =
    compose(fa.run)

  def first[C]: Encoder[(A, C), (B, C)] =
    Encoder.instance { (a, c) => (run(a), c) }

  def second[C]: Encoder[(C, A), (C, B)] =
    Encoder.instance { (c, a) => (c, run(a)) }

object Encoder extends EncoderInstances:
  def apply[A, B](using Encoder[A, B]): Encoder[A, B] =
    summon[Encoder[A, B]]

  def instance[A, B](f: A => B): Encoder[A, B] =
    a => f(a)

  def pure[A, B](b: B): Encoder[A, B] =
    instance(_ => b)

  def ask[A]: Encoder[A, A] =
    instance(a => a)

sealed abstract class EncoderInstances extends EncoderInstances0:
  given [A]: CommutativeMonad[Encoder[A, *]] =
    new CommutativeMonad[Encoder[A, *]]:
      def pure[B](b: B): Encoder[A, B] =
        Encoder.pure(b)

      def flatMap[B, C](fa: Encoder[A, B])(f: B => Encoder[A, C]): Encoder[A, C] =
        fa.flatMap(f)

      def tailRecM[B, C](b: B)(f: B => Encoder[A, Either[B, C]]): Encoder[A, C] =
        Encoder.instance { a =>
          CommutativeMonad[Id].tailRecM(b)(f(_).run(a))
        }

  given CommutativeArrow[Encoder[*, *]] =
    new CommutativeArrow[Encoder[*, *]]:
      def lift[A, B](f: A => B): Encoder[A, B] =
        Encoder.instance(f)

      def first[A, B, C](fa: Encoder[A, B]): Encoder[(A, C), (B, C)] =
        fa.first

      def compose[A, B, C](f: Encoder[B, C], g: Encoder[A, B]): Encoder[A, C] =
        f.compose(g)

  given [B]: Contravariant[Encoder[*, B]] =
    new Contravariant[Encoder[*, B]]:
      def contramap[AA, A](fa: Encoder[AA, B])(f: A => AA): Encoder[A, B] =
        fa.local(f)

sealed abstract class EncoderInstances0:
  given Choice[Encoder[*, *]] =
    new Choice[Encoder[*, *]]:
      def id[A]: Encoder[A, A] =
        Encoder.ask[A]

      def choice[A, B, C](f: Encoder[A, C], g: Encoder[B, C]): Encoder[Either[A, B], C] =
        Encoder.instance(_.fold(f.run, g.run))

      def compose[A, B, C](f: Encoder[B, C], g: Encoder[A, B]): Encoder[A, C] =
        f.compose(g)
