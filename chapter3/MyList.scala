sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(doubles: List[Double]): Double = doubles match {
    case Nil => 1.0
    case Cons(0.0, xs) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  // Exercise 3.2
  def tail[A](as: List[A]): List[A] = as match{
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  // Exercise 3.3
  def setHead[A](as: List[A], a: A): List[A] = as match {
    case Nil => Cons(a, Nil)
    case Cons(h, t) => Cons(a, t)
  }

  // Exercise 3.4
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = if(n == 0) l else drop(tail(l), n-1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    @annotation.tailrec
    def tailRecursionDrop[A](l: List[A], f: A => Boolean, result: List[A]): List[A] = l match {
      case Nil => result
      case Cons(a, as) if f(a) => tailRecursionDrop(as, f, result)
      case Cons(a, as) => tailRecursionDrop(as, f, Cons(a, result))
    }
    tailRecursionDrop(l, f, Nil)
  }  

  def dropWhileSolution2[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(a, as) if f(a) => dropWhileSolution2(as, f)
    case Cons(a, as) => Cons(a, dropWhileSolution2(as, f))
  }

  def dropWhileSolution3[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(a, as) if f(a) => dropWhileSolution3(as)(f)
    case _ => l
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, length) => length + 1)

  def foldLeft[A,B](as: List[A], z: B)(f:(B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((l, x) => Cons(x, l))

  def sumUsingFoldLeft(as: List[Int]): Int = foldLeft(as, 0)(_ + _) 

  // TODO: wrong implementation
  def foldRightUsingFoldLeft[A,B](as: List[A], z:B)(f: (A, B) => B): B = ???

  def append[A](xs: List[A], x: A): List[A] = Cons(x, xs)

  def concatenate[A](xs1: List[A], xs2: List[A]): List[A] = foldRight(xs1, xs2)(Cons(_, _))

  def plusEach(xs: List[Int], n: Int): List[Int] = foldRight(xs, Nil: List[Int])((x, xs) => Cons(x + n, xs))

  def doublesToString(xs: List[Double]): List[String] = foldRight(xs, Nil: List[String])((x, xs) => Cons(x.toString, xs))

  def map[A,B](xs: List[A])(f: A => B): List[B] = foldRight(xs, Nil: List[B])((x, xs) => Cons(f(x), xs))

  // Exercise 3.19
  def filter[A](xs: List[A])(f: A => Boolean): List[A] = foldRight(xs, Nil: List[A])((x, xs) => if(f(x)) Cons(x, xs) else xs)

  // Exercise 3.20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = foldRight(l, Nil: List[B])((x, xs) => concatenate(f(x), xs))

  def zipWith = ???
}

object App {
  def main(args: Array[String]): Unit = {
    val x = List(1, 2, 3, 4, 5)
    println(s"List ${x}")
    println(List.setHead(x, 10))
    println(List.drop(x, 2))

    println(s"Test dropWhile: ${List.dropWhile(x, (y: Int) => y%2 == 0)}")
    println(s"Test dropWhileSolution2: ${List.dropWhileSolution2(x, (y: Int) => y%2 == 0)}")
    println(s"Test dropWhileSolution3: ${List.dropWhileSolution3(x)(x => x % 2 ==0)}")
    println(s"Test length: ${List.length(x)}")

    println(s"Test foldRight ${List.foldRight(x, 0)((x, y) => (x + y)/2)}")
    println(s"Test foldLeft ${List.foldLeft(x, 0)((x, y) => (x + y)/2)}")

    println(s"Test reverse ${List.reverse(x)}")

    println(s"Test sumUsingFoldLeft: ${List.sumUsingFoldLeft(x)}")
//    println(s"Test foldRightUsingFoldLeft ${List.foldRightUsingFoldLeft(x, 0)((x, y) => (x + y)/2)}")

    println(s"Test append ${List.append(x, 4)}")
    println(s"Test concatenate ${List.concatenate(x, List(6,7,8))}")

    println(s"Test plusEach ${List.plusEach(x, 1)}")
    println(s"Test doublesToString ${List.doublesToString(List(1.0, 2.0))}")

    println(s"Test map ${List.map(x)(_*2)}")
    println(s"Test filter ${List.filter(x)(_ % 2 == 0)}")

    println(s"Test flatMap ${List.flatMap(x)(_ => List('h', 'e'))}")
  }
    
}
