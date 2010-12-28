package lists

/*
Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
 */

object P08 {

  def compress(l: List[Symbol]): List[Symbol] = l match {
    case Nil => Nil
    case head::tail => head::compress(tail.dropWhile(_ == head))
  }

  def main(args: Array[String]): Unit = {
    println(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}