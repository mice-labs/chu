package chu.text

import cats.Contravariant
import chu.data.Encoder

/** Namespace for Show Computation type: Conversion from A type to a readable String Encapsulation type: A => String
  * @tparam A
  *   input type
  * @see
  *   https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Text-Show.html
  * @see
  *   https://typelevel.org/cats/typeclasses/show.html
  */
trait Show[-A] extends Encoder[A, String]:
  override def local[B](f: B => A): Show[B] =
    Show.instance(run.compose(f))

  def contramap[B](f: B => A): Show[B] =
    local(f)

  def mapString(f: String => String): Show[A] =
    Show.instance(run.andThen(f))

object Show:
  def apply[A: Show]: Show[A] =
    summon[Show[A]]

  def instance[A](f: A => String): Show[A] =
    a => f(a)

  given Contravariant[Show] =
    new Contravariant[Show]:
      def contramap[AA, A](fa: Show[AA])(f: A => AA): Show[A] =
        fa.local(f)
