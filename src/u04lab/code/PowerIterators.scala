package u04lab.code

import Optionals._
import Lists._
import Lists.List._
import Streams._

import scala.util.Random

trait PowerIterator[A] {
  def next(): Option[A]
  def allSoFar(): List[A]
  def reversed(): PowerIterator[A]
}

trait PowerIteratorsFactory {

  def incremental(start: Int, successive: Int => Int): PowerIterator[Int]
  def fromList[A](list: List[A]): PowerIterator[A]
  def randomBooleans(size: Int): PowerIterator[Boolean]
}

class PowerIteratorsFactoryImpl extends PowerIteratorsFactory {

  private def fromStream[A](stream: Stream[A]) : PowerIterator[A] = {
    var iterator: Stream[A] = stream
    new PowerIterator[A] {
      private var list: List[A] = nil

      override def next(): Option[A] = iterator match {
        case Stream.Empty() => Option.empty
        case Stream.Cons(head, tail) =>
          iterator = tail()
          list = append(list, Cons(head(), Nil()))
          return Option.of(head())
      }

      override def allSoFar(): List[A] = list

      override def reversed(): PowerIterator[A] = fromStream(toStream(reverse(list)))
    }
  }

  def toStream[A](l: List[A]): Stream[A] = l match {
    case Cons(head, tail) => Stream.cons(head, toStream(tail))
    case Nil() => Stream.empty
  }

  override def incremental(start: Int, successive: Int => Int): PowerIterator[Int] = fromStream(Stream.iterate(start)(successive))

  override def fromList[A](list: List[A]): PowerIterator[A] = fromStream(toStream(list))

  override def randomBooleans(size: Int): PowerIterator[Boolean] = fromStream(Stream.take(Stream.generate(Random.nextBoolean()))(size))

  }

