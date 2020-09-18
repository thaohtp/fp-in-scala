sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Leaf {
  def apply[A](value: A): Tree[A] = new Leaf(value)
}

object Branch{
  def apply[A](left: Tree[A], right: Tree[A]): Tree[A] = new Branch(left, right)
}

object Tree{
  // Exercise 3.25
  def size[A](t: Tree[A]): Int = t match{
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  // Exercise 3.26
  def max(t: Tree[Int]): Int = t match{
    case Leaf(value) => value
    case Branch(left, right) => max(left).max(max(right))
  }

  // Exercise 3.27
  def depth[A](t: Tree[A]): Int = t match{
    case Leaf(_) => 1
    case Branch(left, right) => 1 + depth(left).max(depth(right))
  }

  // Exercise 3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match{
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }
}


object App {
  def main(args: Array[String]): Unit = {
    println("haha")
    println(Leaf(1))
    println(Branch(Leaf(1), Leaf(3)))

    val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    println(s"Test size: ${Tree.size(t)}")
    println(s"Test max: ${Tree.max(t)}")
    println(s"Test depth: ${Tree.depth(t)}")
    println(s"Test map: ${Tree.map(t)(_ * 2)}")
  }
}
