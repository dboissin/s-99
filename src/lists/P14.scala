package lists

/*
 Duplicate the elements of a list.
 */

object P14 {

  def duplicate(l: List[Symbol]): List[Symbol] =
    l flatMap (el => List(el, el))

  def main(args: Array[String]): Unit = {
    println(duplicate(List('a, 'b, 'c, 'c, 'd)))
  }
}