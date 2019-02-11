/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
object XX extends App {
  import fs2.{Pipe, Pull, Segment, Stream}

  import cats.effect.{Effect, IO, Timer}

  import scala.concurrent.{ExecutionContext, Future}
  import scala.concurrent.duration._

//  final case class X(x: Long)
//  def test[F[_], O <: X](stream: => Int => Stream[F, O],
//                         pos: Int,
//                         restartDelay: FiniteDuration)(
//                          implicit F: Effect[F],
//                          T: Timer[F]): Stream[F, O] = {
//
//    def go(st: Stream[F, O], p: Int): Pull[F, O, Unit] = {
//      st.pull.uncons.attempt.flatMap[F, O, Unit] {
//        case Right(None) => Pull.pure(None)
//        case Right(Some((h, t))) => {
//          Pull
//            .segment(h.take(2).flatMapResult {
//              case Right(x) =>  Segment.empty.asResult(pos)
//              case Left(err) => Segment.empty.asResult(pos)
//            })
//            .flatMap(go(t, _))
//        }
//        case Left(err) => Pull.eval(T.sleep(restartDelay)) >> go(st, p)
//      }
//    }
//
//    go(stream(pos), pos).stream
//  }
//
//  import scala.concurrent.duration._
//  implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)
//
//  def pipe[F[_], Int]: Pipe[F, Int, Unit] = _.map(r => println(r))
//
//  test(pos => Stream.emits((0 to pos).map(x => X(x.toLong))).covary[IO],
//    100,
//    1.second)
//    .through(pipe)
//    .compile
//    .toList
//    .unsafeRunSync()
  // res32: List[Int] = List(1, 2)

  private val value: DynamicArray[Bytes32] = DynamicArray.empty("te").asInstanceOf[DynamicArray[Bytes32]]


  println(value.getValue.size())
}
