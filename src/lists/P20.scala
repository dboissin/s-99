package lists

/*
 Remove the Kth element from a list.
 Return the list and the removed element in a Tuple. Elements are numbered from 0.
 */

object P20 {

  def removeAt[T](n: Int, l: List[T]): (List[T], T) = (n, l) match {
    case (0, _) => (l.tail, l.head)
    case (_, _) => val (ls,el) = removeAt(n-1, l.tail)
      (l.head::ls, el)
  }

  def main(args: Array[String]): Unit = {
    println(removeAt(1, List('a, 'b, 'c, 'd)))
  }
}