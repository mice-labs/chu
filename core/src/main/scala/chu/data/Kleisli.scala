package chu.data

import cats.*
import cats.arrow.*
import cats.evidence.As
import cats.implicits.*
import chu.*

/** Namespace for Kleisli monad
  *
  * Computation type: Generalized representation of A => F[B]
  *
  * Encapsulation type: A => F[B]
  * @tparam A
  *   input type
  * @tparam B
  *   output type
  */
trait Kleisli[F[_], -A, B]:
  def apply(a: A): F[B] = run(a)
  def run(a: A): F[B]
  def local[C](f: C => A): Kleisli[F, C, B] =
    Kleisli.instance(run.compose(f))

  def ap[C, D, AA <: A](f: Kleisli[F, AA, C])(using F: Apply[F], ev: B As (C => D)): Kleisli[F, AA, D] = {
    Kleisli.instance { a =>
      val fb: F[C => D] = F.map(run(a))(ev.coerce)
      val fc: F[C]      = f.run(a)
      F.ap(fb)(fc)
    }
  }

  def mapFilter[B1](f: B => Option[B1])(using FunctorFilter[F]): Kleisli[F, A, B1] =
    Kleisli.instance(run.andThen(FunctorFilter[F].mapFilter(_)(f)))

  def map[C](f: B => C)(using Functor[F]): Kleisli[F, A, C] =
    mapF(_.map(f))

  def mapF[N[_], C](f: F[B] => N[C]): Kleisli[N, A, C] =
    Kleisli.instance(run.andThen(f))

  def mapK[G[_]](f: F ~> G): Kleisli[G, A, B] = mapF(f.apply)

  def dimap[C, D](f: C => A)(g: B => D)(using Functor[F]): Kleisli[F, C, D] =
    Kleisli.instance(run.compose(f).andThen(Functor[F].map(_)(g)))

  def lmap[C](f: C => A)(using Functor[F]): Kleisli[F, C, B] =
    dimap[C, B](f)(identity)

  def rmap[C](f: B => C)(using Functor[F]): Kleisli[F, A, C] =
    dimap[A, C](identity)(f)

  def flatMap[C, AA <: A](f: B => Kleisli[F, AA, C])(using FlatMap[F]): Kleisli[F, AA, C] =
    Kleisli.shift(a => run(a).flatMap(b => f(b).run(a)))

  def flatMapF[C](f: B => F[C])(using FlatMap[F]): Kleisli[F, A, C] =
    Kleisli.shift(a => run(a).flatMap(f))

  def andThen[C](f: B => F[C])(using FlatMap[F]): Kleisli[F, A, C] =
    flatMapF(f)

  def andThen[C](k: Kleisli[F, B, C])(using FlatMap[F]): Kleisli[F, A, C] =
    andThen(k.run)

  def >>>[C](k: Kleisli[F, B, C])(using FlatMap[F]): Kleisli[F, A, C] =
    andThen(k)

  def compose[Z, AA <: A](f: Z => F[AA])(using FlatMap[F]): Kleisli[F, Z, B] =
    Kleisli.shift(z => FlatMap[F].flatMap(f(z))(run))

  def compose[Z, AA <: A](k: Kleisli[F, Z, AA])(using FlatMap[F]): Kleisli[F, Z, B] =
    compose(k.run)

  def <<<[Z, AA <: A](k: Kleisli[F, Z, AA])(using FlatMap[F]): Kleisli[F, Z, B] =
    compose(k.run)

  def first[C](using Functor[F]): Kleisli[F, (A, C), (B, C)] =
    Kleisli.instance { case (a, c) => Functor[F].fproduct(run(a))(_ => c) }

  def second[C](using Functor[F]): Kleisli[F, (C, A), (C, B)] =
    Kleisli.instance { case (c, a) => Functor[F].map(run(a))(c -> _) }

object Kleisli extends KleisliInstances:
  def instance[F[_], A, B](f: A => F[B]): Kleisli[F, A, B] =
    a => f(a)

  def shift[F[_], A, B](run: A => F[B])(using F: FlatMap[F]): Kleisli[F, A, B] =
    F match {
      case ap: Applicative[F] @unchecked =>
        Kleisli.instance(r => F.flatMap(ap.pure(r))(run))
      case _ =>
        Kleisli.instance(run)
    }
  def liftF[F[_], A, B](fb: F[B]): Kleisli[F, A, B] =
    instance(_ => fb)

  def pure[F[_]: Applicative, A, B](b: B): Kleisli[F, A, B] =
    liftF(Applicative[F].pure(b))

  def ask[F[_], A](using Applicative[F]): Kleisli[F, A, A] =
    Kleisli.instance(Applicative[F].pure)

  def choose[F[_], A, B, C, D](f: Kleisli[F, A, C])(g: Kleisli[F, B, D])(using Functor[F]): Kleisli[F, Either[A, B], Either[C, D]] =
    instance {
      case Left(a)  => Functor[F].map(f(a))(Left.apply)
      case Right(b) => Functor[F].map(g(b))(Right.apply)
    }

  def choice[F[_], A, B, C](f: Kleisli[F, A, C], g: Kleisli[F, B, C])(using Functor[F]): Kleisli[F, Either[A, B], C] =
    choose(f)(g).rmap(_.fold(identity, identity))

sealed abstract class KleisliInstances extends KleisliInstances0:
  given [A]: CommutativeMonad[Kleisli[Id, A, *]] =
    commutativeMonadForKleisli[Id, A]

  given [A]: CommutativeArrow[Kleisli[Id, *, *]] =
    commutativeArrowForKleisli[Id]

  given [F[_]: Defer, A]: Defer[Kleisli[F, A, *]] =
    new Defer[Kleisli[F, A, *]]:
      def defer[B](fa: => Kleisli[F, A, B]): Kleisli[F, A, B] = {
        lazy val cacheFa = fa
        Kleisli.instance(a => Defer[F].defer(cacheFa.run(a)))
      }

  given [F[_]: FunctorFilter, A]: FunctorFilter[Kleisli[F, A, *]] =
    new KleisliFunctorFilter[F, A]:
      def FF: FunctorFilter[F] = FunctorFilter[F]

sealed abstract class KleisliInstances0 extends KleisliInstances1:
  given commutativeArrowForKleisli[F[_]](using CommutativeMonad[F]): CommutativeArrow[Kleisli[F, *, *]] =
    new KleisliCommutativeArrow[F]:
      def F: CommutativeMonad[F] = CommutativeMonad[F]

  given commutativeMonadForKleisli[F[_], A](using CommutativeMonad[F]): CommutativeMonad[Kleisli[F, A, *]] =
    new KleisliMonad[F, A] with CommutativeMonad[Kleisli[F, A, *]]:
      def F: Monad[F] = Monad[F]

sealed abstract class KleisliInstances1 extends KleisliInstances2:
  given [F[_], A, B](using Monoid[F[B]]): Monoid[Kleisli[F, A, B]] =
    new KleisliMonoid[F, A, B]:
      def FB: Monoid[F[B]] = Monoid[F[B]]

  given [F[_], A, E](using MonadError[F, E]): MonadError[Kleisli[F, A, *], E] =
    new KleisliMonadError[F, A, E]:
      def F: MonadError[F, E] = MonadError[F, E]

  given [F[_]: Monad]: ArrowChoice[Kleisli[F, *, *]] =
    new KleisliArrowChoice[F]:
      def F: Monad[F] = Monad[F]

  given [F[_]: ContravariantMonoidal, A]: ContravariantMonoidal[Kleisli[F, A, *]] =
    new KleisliContravariantMonoidal[F, A]:
      def F: ContravariantMonoidal[F] = ContravariantMonoidal[F]

sealed abstract class KleisliInstances2 extends KleisliInstances3:
  given monadForKleisli[F[_]: Monad, A]: Monad[Kleisli[F, A, *]] =
    new KleisliMonad[F, A]:
      def F: Monad[F] = Monad[F]

  given [M[_], A](using P: Parallel[M]): Parallel.Aux[Kleisli[M, A, *], Kleisli[P.F, A, *]] =
    new Parallel[Kleisli[M, A, *]] {
      type F[x] = Kleisli[P.F, A, x]
      implicit val monadM: Monad[M]                    = P.monad
      def applicative: Applicative[Kleisli[P.F, A, *]] = applicativeForKleisli[P.F, A](P.applicative)

      def monad: Monad[Kleisli[M, A, *]] = monadForKleisli

      def sequential: Kleisli[P.F, A, *] ~> Kleisli[M, A, *] =
        new (Kleisli[P.F, A, *] ~> Kleisli[M, A, *]) {
          def apply[B](k: Kleisli[P.F, A, B]): Kleisli[M, A, B] = k.mapK(P.sequential)
        }

      def parallel: Kleisli[M, A, *] ~> Kleisli[P.F, A, *] =
        new (Kleisli[M, A, *] ~> Kleisli[P.F, A, *]) {
          def apply[B](k: Kleisli[M, A, B]): Kleisli[P.F, A, B] = k.mapK(P.parallel)
        }
    }

  given [F[_], B]: Contravariant[Kleisli[F, *, B]] =
    new Contravariant[Kleisli[F, *, B]]:
      override def contramap[AA, A](fa: Kleisli[F, AA, B])(f: A => AA): Kleisli[F, A, B] =
        fa.local(f)

sealed abstract class KleisliInstances3 extends KleisliInstances4:
  given [F[_]: Alternative, A]: Alternative[Kleisli[F, A, *]] =
    new KleisliAlternative[F, A]:
      def F: Alternative[F] = Alternative[F]

sealed abstract class KleisliInstances4 extends KleisliInstances5:
  given [F[_]: MonoidK, A]: MonoidK[Kleisli[F, A, *]] =
    new KleisliMonoidK[F, A]:
      def F: MonoidK[F] = MonoidK[F]

  given [F[_]: CommutativeFlatMap, A]: CommutativeFlatMap[Kleisli[F, A, *]] =
    new KleisliFlatMap[F, A] with CommutativeFlatMap[Kleisli[F, A, *]]:
      def F: CommutativeFlatMap[F] = CommutativeFlatMap[F]

  given choiceForKleisli[F[_]: Monad]: Choice[Kleisli[F, *, *]] =
    new KleisliChoice[F]:
      def F: Monad[F] = Monad[F]

  given Choice[Kleisli[Id, *, *]] =
    choiceForKleisli[Id]

  given [F[_]: FlatMap, A]: Compose[Kleisli[F, *, *]] =
    new KleisliCompose[F]:
      def F: FlatMap[F] = FlatMap[F]

  given [F[_]: Functor, A]: Strong[Kleisli[F, *, *]] =
    new KleisliStrong[F]:
      def F: Functor[F] = Functor[F]

  given [F[_], A, B](using Semigroup[F[B]]): Semigroup[Kleisli[F, A, B]] =
    new KleisliSemigroup[F, A, B]:
      def FB: Semigroup[F[B]] = Semigroup[F[B]]

sealed abstract class KleisliInstances5 extends KleisliInstances6:
  given [F[_]: SemigroupK, A]: SemigroupK[Kleisli[F, A, *]] =
    new KleisliSemigroupK[F, A]:
      def F: SemigroupK[F] = SemigroupK[F]
  given [F[_]: FlatMap, A]: FlatMap[Kleisli[F, A, *]] =
    new KleisliFlatMap[F, A]:
      def F: FlatMap[F] = FlatMap[F]
sealed abstract class KleisliInstances6 extends KleisliInstances7:
  given [F[_], A, E](using ApplicativeError[F, E]): ApplicativeError[Kleisli[F, A, *], E] =
    new KleisliApplicativeError[F, A, E]:
      def F: ApplicativeError[F, E] = ApplicativeError[F, E]

sealed abstract class KleisliInstances7 extends KleisliInstances8:
  given applicativeForKleisli[F[_]: Applicative, A]: Applicative[Kleisli[F, A, *]] =
    new KleisliApplicative[F, A]:
      def F: Applicative[F] = Applicative[F]

sealed abstract class KleisliInstances8 extends KleisliInstances9:
  given [F[_]: Apply, A]: Apply[Kleisli[F, A, *]] =
    new KleisliApply[F, A]:
      def F: Apply[F] = Apply[F]

sealed abstract class KleisliInstances9 extends KleisliInstances10:
  given [F[_]: Distributive, A]: Distributive[Kleisli[F, A, *]] =
    new KleisliDistributive[F, A]:
      def F: Distributive[F] = Distributive[F]

sealed abstract class KleisliInstances10:
  given [F[_]: Functor, A]: Functor[Kleisli[F, A, *]] =
    new KleisliFunctor[F, A]:
      def F: Functor[F] = Functor[F]

trait KleisliCommutativeArrow[F[_]] extends CommutativeArrow[Kleisli[F, *, *]] with KleisliArrowChoice[F] {
  implicit def F: CommutativeMonad[F]
}

private trait KleisliContravariantMonoidal[F[_], A] extends ContravariantMonoidal[Kleisli[F, A, *]]:
  implicit def F: ContravariantMonoidal[F]

  override def unit: Kleisli[F, A, Unit] = Kleisli.instance(Function.const(F.unit))

  override def contramap[B, C](fa: Kleisli[F, A, B])(f: C => B): Kleisli[F, A, C] =
    Kleisli.instance(a => F.contramap(fa.run(a))(f))

  override def product[B, C](fa: Kleisli[F, A, B], fb: Kleisli[F, A, C]): Kleisli[F, A, (B, C)] =
    Kleisli.instance(a => F.product(fa.run(a), fb.run(a)))

private trait KleisliMonadError[F[_], A, E] extends MonadError[Kleisli[F, A, *], E] with KleisliApplicativeError[F, A, E] with KleisliMonad[F, A] {
  def F: MonadError[F, E]
}

private trait KleisliMonoid[F[_], A, B] extends Monoid[Kleisli[F, A, B]] with KleisliSemigroup[F, A, B]:
  implicit def FB: Monoid[F[B]]

  override def empty: Kleisli[F, A, B] = Kleisli.instance(_ => FB.empty)

trait KleisliArrowChoice[F[_]] extends ArrowChoice[Kleisli[F, *, *]] with KleisliCategory[F] with KleisliStrong[F] {
  implicit def F: Monad[F]

  def lift[A, B](f: A => B): Kleisli[F, A, B] =
    Kleisli.instance(a => F.pure(f(a)))

  override def split[A, B, C, D](f: Kleisli[F, A, B], g: Kleisli[F, C, D]): Kleisli[F, (A, C), (B, D)] =
    Kleisli.instance { case (a, c) => F.flatMap(f.run(a))(b => F.map(g.run(c))(d => (b, d))) }

  def choose[A, B, C, D](f: Kleisli[F, A, C])(g: Kleisli[F, B, D]): Kleisli[F, Either[A, B], Either[C, D]] =
    Kleisli.instance {
      case Left(a)  => F.map(f(a))(Left.apply)
      case Right(b) => F.map(g(b))(Right.apply)
    }
}

private trait KleisliMonad[F[_], A] extends Monad[Kleisli[F, A, *]] with KleisliFlatMap[F, A] with KleisliApplicative[F, A] {
  implicit def F: Monad[F]
}

private trait KleisliAlternative[F[_], A] extends Alternative[Kleisli[F, A, *]] with KleisliApplicative[F, A] with KleisliMonoidK[F, A]:
  implicit def F: Alternative[F]

private trait KleisliSemigroup[F[_], A, B] extends Semigroup[Kleisli[F, A, B]]:
  implicit def FB: Semigroup[F[B]]

  override def combine(a: Kleisli[F, A, B], b: Kleisli[F, A, B]): Kleisli[F, A, B] =
    Kleisli.instance(x => FB.combine(a.run(x), b.run(x)))

private trait KleisliStrong[F[_]] extends Strong[Kleisli[F, *, *]] {
  implicit def F: Functor[F]

  override def lmap[A, B, C](fab: Kleisli[F, A, B])(f: C => A): Kleisli[F, C, B] =
    fab.local(f)

  override def rmap[A, B, C](fab: Kleisli[F, A, B])(f: B => C): Kleisli[F, A, C] =
    fab.map(f)

  override def dimap[A, B, C, D](fab: Kleisli[F, A, B])(f: C => A)(g: B => D): Kleisli[F, C, D] =
    fab.dimap(f)(g)

  def first[A, B, C](fa: Kleisli[F, A, B]): Kleisli[F, (A, C), (B, C)] =
    fa.first[C]

  override def second[A, B, C](fa: Kleisli[F, A, B]): Kleisli[F, (C, A), (C, B)] =
    fa.second[C]
}
private trait KleisliChoice[F[_]] extends Choice[Kleisli[F, *, *]] with KleisliCategory[F] {
  def choice[A, B, C](f: Kleisli[F, A, C], g: Kleisli[F, B, C]): Kleisli[F, Either[A, B], C] =
    Kleisli.instance(_.fold(f.run, g.run))
}

private trait KleisliCategory[F[_]] extends Category[Kleisli[F, *, *]] with KleisliCompose[F] {
  implicit def F: Monad[F]

  override def id[A]: Kleisli[F, A, A] = Kleisli.ask[F, A]
}

private trait KleisliCompose[F[_]] extends Compose[Kleisli[F, *, *]] {
  implicit def F: FlatMap[F]

  def compose[A, B, C](f: Kleisli[F, B, C], g: Kleisli[F, A, B]): Kleisli[F, A, C] =
    f.compose(g)
}

private trait KleisliMonoidK[F[_], A] extends MonoidK[Kleisli[F, A, *]] with KleisliSemigroupK[F, A]:
  implicit def F: MonoidK[F]

  override def empty[B]: Kleisli[F, A, B] = Kleisli.liftF(F.empty[B])

private trait KleisliSemigroupK[F[_], A] extends SemigroupK[Kleisli[F, A, *]]:
  implicit def F: SemigroupK[F]

  override def combineK[B](x: Kleisli[F, A, B], y: Kleisli[F, A, B]): Kleisli[F, A, B] =
    Kleisli.instance(a => F.combineK(x.run(a), y.run(a)))

  override def combineKEval[B](x: Kleisli[F, A, B], y: Eval[Kleisli[F, A, B]]): Eval[Kleisli[F, A, B]] =
    Eval.now(Kleisli.instance(a => F.combineKEval(x.run(a), y.map(_.run(a))).value))

private trait KleisliFlatMap[F[_], A] extends FlatMap[Kleisli[F, A, *]] with KleisliApply[F, A] {
  implicit def F: FlatMap[F]

  def flatMap[B, C](fa: Kleisli[F, A, B])(f: B => Kleisli[F, A, C]): Kleisli[F, A, C] =
    fa.flatMap(f)

  def tailRecM[B, C](b: B)(f: B => Kleisli[F, A, Either[B, C]]): Kleisli[F, A, C] =
    Kleisli.instance { a =>
      F.tailRecM(b)(f(_).run(a))
    }
}
private trait KleisliApplicativeError[F[_], A, E] extends ApplicativeError[Kleisli[F, A, *], E] with KleisliApplicative[F, A]:
  implicit def F: ApplicativeError[F, E]

  def raiseError[B](e: E): Kleisli[F, A, B] =
    Kleisli.instance(_ => F.raiseError(e))

  def handleErrorWith[B](fb: Kleisli[F, A, B])(f: E => Kleisli[F, A, B]): Kleisli[F, A, B] =
    Kleisli.instance { a =>
      F.handleErrorWith(fb.run(a))(e => f(e).run(a))
    }

private trait KleisliApplicative[F[_], A] extends Applicative[Kleisli[F, A, *]] with KleisliApply[F, A]:
  implicit def F: Applicative[F]

  def pure[B](x: B): Kleisli[F, A, B] =
    Kleisli.pure[F, A, B](x)

private trait KleisliApply[F[_], A] extends Apply[Kleisli[F, A, *]] with KleisliFunctor[F, A]:
  implicit def F: Apply[F]

  override def ap[AA, B](ff: Kleisli[F, A, AA => B])(fa: Kleisli[F, A, AA]): Kleisli[F, A, B] =
    ff.ap(fa)

  override def map2Eval[B, C, Z](fa: Kleisli[F, A, B], fb: Eval[Kleisli[F, A, C]])(
      f: (B, C) => Z
  ): Eval[Kleisli[F, A, Z]] = {
    // We should only evaluate fb once
    val memoFb = fb.memoize

    Eval.now(Kleisli.instance { a =>
      val fb              = fa.run(a)
      val efc             = memoFb.map(_.run(a))
      val efz: Eval[F[Z]] = F.map2Eval(fb, efc)(f)
      // This is not safe and results in stack overflows:
      // see: https://github.com/typelevel/cats/issues/3947
      efz.value
    })
  }

  override def product[B, C](fb: Kleisli[F, A, B], fc: Kleisli[F, A, C]): Kleisli[F, A, (B, C)] =
    Kleisli.instance(a => Apply[F].product(fb.run(a), fc.run(a)))

private trait KleisliDistributive[F[_], A] extends Distributive[Kleisli[F, A, *]]:
  implicit def F: Distributive[F]
  def distribute[G[_]: Functor, AA, B](ga: G[AA])(f: AA => Kleisli[F, A, B]): Kleisli[F, A, G[B]] =
    Kleisli.instance(a => Distributive[F].distribute(ga)(f(_).run(a)))

  def map[AA, B](fa: Kleisli[F, A, AA])(f: AA => B): Kleisli[F, A, B] =
    fa.map(f)

private trait KleisliFunctor[F[_], A] extends Functor[Kleisli[F, A, *]]:
  implicit def F: Functor[F]

  override def map[B, C](fa: Kleisli[F, A, B])(f: B => C): Kleisli[F, A, C] =
    fa.map(f)

  override def void[B](fa: Kleisli[F, A, B]): Kleisli[F, A, Unit] =
    Kleisli.instance(fa.run.andThen(Functor[F].void))

trait KleisliFunctorFilter[F[_], A] extends FunctorFilter[Kleisli[F, A, *]] {

  def FF: FunctorFilter[F]

  def functor: Functor[Kleisli[F, A, *]] =
    new KleisliFunctor[F, A]:
      def F: Functor[F] = FF.functor

  def mapFilter[B, C](fa: Kleisli[F, A, B])(f: B => Option[C]): Kleisli[F, A, C] =
    fa.mapFilter(f)(using FF)
}
