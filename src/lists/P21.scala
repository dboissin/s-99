package lists

/*
Insert an element at a given position into a list.
 */

object P21 {

  def insertAt[T](el: T, n: Int, l: List[T]): List[T] = (n,l) match {
    case (_, Nil) => Nil
    case (0, _) => el::l
    case (_, _) => l.head::insertAt (el, n-1, l.tail)
  }

  def main(args: Array[String]): Unit = {
    println(insertAt('new, 1, List('a, 'b, 'c, 'd)))
  }
}