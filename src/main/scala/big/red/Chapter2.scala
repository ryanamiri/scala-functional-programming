package big.red

import scala.annotation.tailrec

object Chapter2 extends App {

  // Write iterative solution first to come up with recursive one
  def fib_iterative(n: Int): Int = {
    var x = n
    var (a, b) = (0, 1)
    while(x > 1) {
      var c = b
      b = a + b
      a = c
      x -= 1
    }
    a
  }

  // See the similarity! This is general
  def fib(n: Int): Int = {
    @scala.annotation.tailrec
    def go(first: Int, second: Int, count: Int): Int = {
      if(count == 1) first
      else go(second, first+second, count-1)
    }
    go(0,1,n)
  }

  def isSorted_iterative[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    var i = as.length
    var ans = true
    while (i > 0) {
      if(!ordered(as(i), as(i-1))){
        ans = false
      }
      i = i-1
    }
    ans
  }

  def isSorted[A](as: Array[A])(ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(i: Int): Boolean = {
      if(i == 0) true
      else if(!ordered(as(i), as(i-1))) false
      else go(i-1)
    }
    go(as.length-1)
  }

  println(isSorted(Array(1, 3, 2, 4))((x, y) => x > y))

  def curry[A, B, C](f:(A, B) => C): A => B => C = a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)







}
