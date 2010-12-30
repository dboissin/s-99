package lists

/*
  Rotate a list N places to the left.
 */

object P19 {

  def rotate[T](offset: Int, l: List[T]): List[T] = {
    val (l1, l2) = l.splitAt(if (offset < 0) l.length + offset else offset)
    l2:::l1
  }

  def main(args: Array[String]):Unit = {
    println(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }
}