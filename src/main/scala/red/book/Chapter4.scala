package red.book

import scala.{Option => _, Either => _, _}

object Chapter4 extends App {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def getOrElse[B >: A](default: => B) : B = this match {
      case None => default
      case Some(b) => b
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

    def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

    def filter(f: A => Boolean): Option[A] = flatMap(a => if(f(a)) Some(a) else None)
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] = xs match {
    case Nil => None
    case ys => Some(ys.sum / ys.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap(m => mean(xs.map( x => math.pow(x-m,2))))

  def map2[A, B, C](ao: Option[A], bo: Option[B])(f: (A, B) => C): Option[C] = ao flatMap(a => bo map(f(a, _)))

  def sequence[A](al: List[Option[A]]): Option[List[A]] =
    al.foldRight[Option[List[A]]](Some(List[A]()))((ao, bo) => map2(ao, bo)((a, b) => a :: b))

  def traverse[A, B](al: List[A])(f: A => Option[B]): Option[List[B]] = al.foldRight[Option[List[B]]](Some(Nil))((a, b) => map2(f(a), b)(_ :: _))

  def seq2[A](al: List[Option[A]]): Option[List[A]] = traverse(al)(a => a)

}
