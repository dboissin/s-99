package lists

/*
  Extract a slice from a list.
  Given two indices, I and K, the slice is the list containing the elements from and
  including the Ith element up to but not including the Kth element of the original list.
  Start counting the elements with 0.
 */

object P18 {

  def slice[T](i: Int, k: Int, l: List[T]): List[T] = (i, k, l) match {
    case (_, _, Nil) => Nil
    case (_, 1, _) => l.head::Nil
    case (0, _, _) => l.head::slice(0, k-1, l.tail)
    case (_, _, _) => slice(i-1, k-1, l.tail)
  }

  def main(args: Array[String]):Unit = {
    println(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }
}