package big.red

object Chapter6 extends App {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {
    case class Simple(seed: Long) extends RNG {
      def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
        val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
        val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
        (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      }
    }


  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    (if(i<0) -(i+1) else i, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    (i.toDouble / (Int.MaxValue + 1), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i,d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng:RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0)
      (List(), rng)
    else {
      val (x, r1)  = rng.nextInt
      val (xs, r2) = ints(count - 1)(r1)
      (x :: xs, r2)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  val int: Rand[Int] = _.nextInt

  def map[A, B](n: Rand[A])(f: A => B): Rand[B] = {
    (r: RNG) => {
      val (a, r2) = n(r)
      (f(a), r2)
    }
  }

  def _double(rng: RNG): Rand[Double] = {
    map(nonNegativeInt)(_ / (Int.MaxValue + 1))
  }

  def map2[A, B, C](a: Rand[A], b: Rand[B])(f: (A, B) => C): Rand[C] = {
    r => {
      val (a1, r1) = a(r)
      val (b1, r2) = b(r1)
      (f(a1, b1), r2)
    }
  }

  def sequence[A](as: List[Rand[A]]): Rand[List[A]] = {
    as.foldRight( (r: RNG) => (List.empty[A], r) )( (ra, ras) => map2(ra, ras)(_ :: _) )
  }

  def flatMap[A, B](n: Rand[A])(f: A => Rand[B]): Rand[B] = {
    (r: RNG) => {
      val (a, r2) = n(r)
      f(a)(r2)
    }
  }

  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

//  type State[S, +A] = S => (A, S)

  case class State[S, +A](run: S => (A, S)){
    def flatMap[B](f: A => State[S, B]): State[S, B] = State((s: S) => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })
    def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))
    def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
      flatMap(a => sb.map(b => f(a, b)))
    }
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def sequence[S, A](as: List[State[S, A]]): State[S, List[A]] = {
      as.foldRight( State[S, List[A]](s => (List.empty[A], s) ))( (state, states) => state.map2(states)(_ :: _) )
    }

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    } yield ()

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  }

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, coins: Int, candies: Int)

  object Candy {

    def handleCoin: State[Machine, (Int, Int)] = {
      State(s => s match {
        case m @ Machine(locked, coins, 0) => ((coins, 0), m)
        case Machine(true, coins, candies) => ((coins+1, candies), Machine(false, coins+1, candies))
        case _ => ((s.coins, s.candies), s)
      })
    }

    def handleTurn: State[Machine, (Int, Int)] = {
      State(s => s match {
        case t @ Machine(false, coins, candy) if candy > 0 => ((coins, candy-1), t.copy(locked = true, candies = candy-1))
        case _ => ((s.coins, s.candies), s)
      })
    }

    def handleInput(i: Input): State[Machine, (Int, Int)] = i match {
      case Coin => handleCoin
      case Turn => handleTurn
    }

    def simulateMachine(inputs: List[Input]): State[Machine, List[(Int, Int)]] = {
      State.sequence(inputs.map(handleInput))
    }


    def update = (i: Input) => (s: Machine) =>
      (i, s) match {
        case (_, Machine(_, _, 0)) => s
        case (Coin, Machine(false, _, _)) => s
        case (Turn, Machine(true, _, _)) => s
        case (Coin, Machine(true, coin, candy)) =>
          Machine(false, coin + 1, candy)
        case (Turn, Machine(false, coin, candy)) =>
          Machine(true, coin, candy - 1)
      }

    def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = for {
      _ <- State.sequence(inputs.map(State.modify[Machine] _ compose update))
      s <- State.get
    } yield (s.coins, s.candies)
  }

  println(Candy.simulateMachine(List(Coin, Coin, Turn, Turn, Coin, Turn, Coin, Turn)).run(Machine(true, 10, 2)))

}
