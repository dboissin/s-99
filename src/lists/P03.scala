package lists

/*
  Find the Kth element of a list.
  By convention, the first element in the list is element 0.
 */

object P03 {

  def nth[T](n: Int, l: List[T]): T = l match {
    case head::tail => if (n == 0) head else nth(n-1, tail)
    case _ => throw new NoSuchElementException
  }

  def main(args : Array[String]) : Unit = {
    println(nth(2, List(1, 1, 2, 3, 5, 8)))
    println(nth(0, List(1, 1, 2, 3, 5, 8)))
    println(nth(4, List(1, 1, 2, 3, 5, 8)))
    println(nth(5, List(1, 1, 2, 3, 5, 8)))
  }
}