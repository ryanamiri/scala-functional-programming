package red.book

import scala.annotation.tailrec

object Chapter3 extends App {

  val x = List(1, 2, 3, 4, 5) match {
    case x :: 2 :: 4 :: _ => x
    case Nil => 42
    case x :: y :: 3 :: 4 :: _ => x + y
    case h :: t => h + t.sum
    case _ => 101
  }

  //  println(x)

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case _ :: t => t
  }

  def setHead[A](xs: List[A], a: A): List[A] = a :: tail(xs)

  @scala.annotation.tailrec
  def drop[A](xs: List[A], n: Int): List[A] = xs match {
    case Nil => Nil
    case _ :: t => drop(t, n - 1)
  }

  @scala.annotation.tailrec
  def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = xs match {
    case h :: t if f(h) => dropWhile(t, f)
    case _ => xs
  }

  def init[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case h :: Nil => Nil
    case h :: t => h :: init(t)
  }

//  println (init (List (1, 2, 3, 4) ) )

  def length[A](xs: List[A]): Int = xs.foldRight(0)((_, b) => b + 1)

  def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = {
    xs match {
      case Nil => z
      case h :: t => foldLeft(t, f(z, h))(f)
    }
  }

  def reverse[A](xs: List[A]): List[A] = foldLeft(xs, Nil:List[A])( (as, a) => a :: as )

//  println(reverse(List(1,2,3)))

  def add1(xs: List[Int]): List[Int] = xs.foldRight(Nil: List[Int])((x,b) => (x+1) :: b)
  def doubleToString(xs: List[Double]): List[String] = xs.foldRight(Nil: List[String])((x, b) => (x.toString) :: b)

  def map[A, B](xs: List[A])(f: A => B): List[B] = xs.foldRight(Nil: List[B])((a, bs) => f(a) :: bs)
  def filter[A](xs: List[A])(f: A => Boolean): List[A] = xs.foldRight(Nil: List[A])((a, bs) => if(f(a)) a :: bs else bs)

//  println(filter(List(1, 2, 3, 4, 5, 6))(x => if(x % 2 == 0) true else false))

  def append[A](l: List[A], r: List[A]): List[A] = l.foldRight(r)(_ :: _)
  def concat[A](l: List[List[A]]): List[A] = l.foldRight(Nil:List[A])(append)
  def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = xs.foldRight(Nil: List[B])((x, bs) => concat(List(f(x), bs)))

//  println(flatMap(List(1,2,3))(x => List(x,x)))

  def hasSubsequence_iterative[A](sup: List[A], sub: List[A]): Boolean = {
    var i = sub.length
    var j = sup
    var ans = false
    while (j.length >= i) {
      if(j.slice(0,i) == sub) ans = true
      j = j.tail
    }
    ans
  }

  @scala.annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    if(sup.length < sub.length) false
    else if(sup.slice(0, sub.length) == sub) true
    else hasSubsequence(sup.tail, sub)
  }

//  println(hasSubsequence(List(1,2,3,4), List(1,2)))
//  println(hasSubsequence(List(1,2,3,4), List(2,3)))
//  println(hasSubsequence(List(1,2,3,4), List(4)))
//  println(hasSubsequence(List(1,2,3,4), List(1,3)))

  sealed trait Tree[+A]
  case class Leaf[A](a: A) extends Tree[A]
  case class Branch[A](a1: Tree[A], a2: Tree[A]) extends Tree[A]

  def size[A](x: Tree[A]): Int = x match {
    case Leaf(_) => 1
    case Branch(a1, a2) => 1 + size(a1) + size(a2)
  }

  def max(x: Tree[Int]): Int = x match {
    case Leaf(a) => a
    case Branch(l, r) => max(l).max(max(r))
  }

  def depth[A](x: Tree[A]): Int = x match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  def map[A, B](x: Tree[A])(f: A => B): Tree[B] = x match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(g: A => B)(f: (B, B) => B): B = t match {
    case Leaf(a) => g(a)
    case Branch(l, r) => f(fold(l)(g)(f), fold(r)(g)(f))
  }

  def size_fold[A](x: Tree[A]): Int = fold(x)(_ => 1)(1 + _ + _)
  def max_fold(x: Tree[Int]): Int = fold(x)(x => x)(_ max _)

  val test = Branch[Int](Branch(Leaf(1), Branch(Leaf(2), Leaf(4))), Leaf(3))
  println(size(test))
  println(size_fold(test))
  println(max(test))
  println(max_fold(test))
  println(depth(test))
  println(map(test)(_ + 1))
}
