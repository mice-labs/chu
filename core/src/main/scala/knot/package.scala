package object knot:
  type Decoder[E, A, B] = data.Decoder[E, A, B]
  val Decoder = data.Decoder
  type Encoder[A, B] = data.Encoder[A, B]
  val Encoder = data.Encoder
  type Kleisli[F[_], A, B] = data.Kleisli[F, A, B]
  val Kleisli = data.Kleisli
  type Reader[E, A] = data.Reader[E, A]
  val Reader = data.Reader
