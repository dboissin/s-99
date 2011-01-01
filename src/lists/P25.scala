package lists

/*
  Generate a random permutation of the elements of a list.
 */

object P25 {
  import P23.randomSelect

  def randomPermute[T](l: List[T]): List[T] =
    randomSelect(l.length, l)

  def main(args: Array[String]): Unit = {
    println(randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)))
  }
}