package object chu:
  type Decoder[E, A, B] = data.Decoder[E, A, B]
  val Decoder = data.Decoder
  type Deserializer[F[_], A, B] = data.Deserializer[F, A, B]
  val Deserializer = data.Deserializer
  type Encoder[A, B] = data.Encoder[A, B]
  val Encoder = data.Encoder
  type Kleisli[F[_], A, B] = data.Kleisli[F, A, B]
  val Kleisli = data.Kleisli
  type Pickle[F[_], A] = data.Pickle[F, A]
  val Pickle = data.Pickle
  type Reader[E, A] = data.Reader[E, A]
  val Reader = data.Reader
  type Serializer[F[_], A, B] = data.Serializer[F, A, B]
  val Serializer = data.Serializer
  type Unpickle[F[_], A] = data.Unpickle[F, A]
  val Unpickle = data.Unpickle
