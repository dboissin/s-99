package lists

/*
  Find the last but one element of a list.
 */

object P02 {

  def penultimate[T](l: List[T]) : T = l match {
    case head::_::Nil => head
    case _::tail => penultimate(tail)
    case Nil => throw new NoSuchElementException
  }

  def main(args : Array[String]) : Unit = {
    println(penultimate(List(1, 1, 2, 3, 5, 8)))
  }
}