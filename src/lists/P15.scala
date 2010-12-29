package lists

/*
 Duplicate the elements of a list a given number of times.
 */

object P15 {

  def duplicateN(nb: Int, l: List[Symbol]): List[Symbol] =
    l flatMap (el => List.fill(nb)(el))

  def main(args: Array[String]): Unit = {
    println(duplicateN(3, List('a, 'b, 'c, 'c, 'd)))
  }
}