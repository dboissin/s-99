package lists

/**
 Reverse a list.
 */

object P05 {

  def reverse[T](l: List[T]): List[T] = //{
    //if (l.tail == Nil)
      //l.head::Nil
   // else
   //  reverse(l.tail):::List(l.head)
   l match {
      case head::Nil => head::Nil
      case head::tail => reverse(tail):::List(head)
      case _ => throw new NoSuchElementException
  }
  def main(args: Array[String]): Unit =
    println(reverse(List(1, 1, 2, 3, 5, 8)))
}