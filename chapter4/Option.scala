sealed trait Option[+A] {
  def map[B](f: A => B) : Option[B] = this match {
    case Some(a) => Some(f(a)) 
    case None => None
  }
  def flatMap[B](f: A => Option[B]) : Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }
  def getOrElse[B >: A](default: => B) : B = this match {
    case None => default
    case Some(a) => a
  }
  def orElse[B >: A](ob: => Option[B]) : Option[B] = if(this == None) ob else this

  def filter[B](f: A => Boolean) : Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object App {
  def main(args: Array[String]): Unit = {
    val opt = Some("hello")

    println(s"Sample Option: ${opt}") 
    
    println(s"Test map: ${opt.map(x => 1)}")
    
    val testFlatMap = opt.flatMap(x => if(x == "hello") Some(1) else None)
    println(s"Test flatMap: ${testFlatMap}")

    val none = None
    val testFlatMapReturnNone = none.flatMap(x => if(x == "hello") Some(1) else Some(2))
    println(s"Test flatMap with None: ${testFlatMapReturnNone}")

    println(s"Test getOrElse - Get wins: ${opt.getOrElse("hallo")}")
    println(s"Test getOrElse - Else wins ${none.getOrElse("hallo")}")

    println(s"Test orElse - current object wins: ${opt.orElse(Some("hallo"))}")
    println(s"Test orElse - Else wins: ${none.orElse(Some("hallo"))}")

    println(s"Test filter with true condition: ${opt.filter(x => x == "hello")} ")
    println(s"Test filter with false condition: ${opt.filter(x => x == "hichic")} ")
  }
}
