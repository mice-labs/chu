package knot.data

import cats.arrow.{Choice, CommutativeArrow}
import cats.{CommutativeMonad, Contravariant, Id}
import knot.data.Kleisli

/** Namespace for Reader monad
  *
  * Computation type: Computation which reads values from a shared environment
  *
  * Encapsulation type: A => B
  * @tparam E
  *   environment type
  * @tparam A
  *   output type
  * @see
  *   https://hackage-content.haskell.org/package/mtl-2.3.2/docs/Control-Monad-Reader.html
  * @see
  *   https://typelevel.org/cats/datatypes/kleisli.html
  */
trait Reader[-E, A] extends Kleisli[Id, E, A]:
  override def local[C](f: C => E): Reader[C, A] =
    Reader.instance(run.compose(f))

  def map[C](f: A => C): Reader[E, C] =
    Reader.instance(run.andThen(f))

  def dimap[C, D](f: C => E)(g: A => D): Reader[C, D] =
    Reader.instance(f.andThen(run).andThen(g))

  def lmap[C](f: C => E): Reader[C, A] =
    local(f)

  def rmap[C](f: A => C): Reader[E, C] =
    map(f)

  def flatMap[C, AA <: E](f: A => Reader[AA, C]): Reader[AA, C] =
    Reader.instance(a => f(run(a)).run(a))

  def andThen[C](f: A => C): Reader[E, C] =
    map(f)

  def andThen[C](fa: Reader[A, C]): Reader[E, C] =
    andThen(fa.run)

  def >>>[C](fa: Reader[A, C]): Reader[E, C] =
    andThen(fa.run)

  def compose[C, AA <: E](f: C => AA): Reader[C, A] =
    local(f)

  def compose[C, AA <: E](fa: Reader[C, AA]): Reader[C, A] =
    compose(fa.run)

  def <<<[C, AA <: E](fa: Reader[C, AA]): Reader[C, A] =
    compose(fa.run)

  def first[C]: Reader[(E, C), (A, C)] =
    Reader.instance { (a, c) => (run(a), c) }

  def second[C]: Reader[(C, E), (C, A)] =
    Reader.instance { (c, a) => (c, run(a)) }

object Reader extends ReaderInstances:
  def apply[A, B](using Reader[A, B]): Reader[A, B] =
    summon[Reader[A, B]]

  def instance[A, B](f: A => B): Reader[A, B] =
    a => f(a)

  def pure[A, B](b: B): Reader[A, B] =
    instance(_ => b)

  def ask[A]: Reader[A, A] =
    a => a

sealed abstract class ReaderInstances extends ReaderInstances0:
  given [A]: CommutativeMonad[Reader[A, *]] =
    new CommutativeMonad[Reader[A, *]]:
      def pure[B](b: B): Reader[A, B] =
        Reader.pure(b)

      def flatMap[B, C](fa: Reader[A, B])(f: B => Reader[A, C]): Reader[A, C] =
        fa.flatMap(f)

      def tailRecM[B, C](b: B)(f: B => Reader[A, Either[B, C]]): Reader[A, C] =
        Reader.instance { a =>
          CommutativeMonad[Id].tailRecM(b)(f(_).run(a))
        }

  given CommutativeArrow[Reader[*, *]] =
    new CommutativeArrow[Reader[*, *]]:
      def lift[A, B](f: A => B): Reader[A, B] =
        Reader.instance(f)

      def first[A, B, C](fa: Reader[A, B]): Reader[(A, C), (B, C)] =
        fa.first

      def compose[A, B, C](f: Reader[B, C], g: Reader[A, B]): Reader[A, C] =
        f.compose(g)

  given [F[_], B]: Contravariant[Reader[*, B]] =
    new Contravariant[Reader[*, B]]:
      def contramap[AA, A](fa: Reader[AA, B])(f: A => AA): Reader[A, B] =
        fa.local(f)

sealed abstract class ReaderInstances0:
  given Choice[Reader[*, *]] =
    new Choice[Reader[*, *]]:
      def id[A]: Reader[A, A] =
        Reader.ask[A]

      def choice[A, B, C](f: Reader[A, C], g: Reader[B, C]): Reader[Either[A, B], C] =
        Reader.instance(_.fold(f.run, g.run))

      def compose[A, B, C](f: Reader[B, C], g: Reader[A, B]): Reader[A, C] =
        f.compose(g)
