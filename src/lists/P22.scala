package lists

/*
  Create a list containing all integers within a given range.
 */

object P22 {

  def range(curr: Int, end: Int): List[Int] = {
    if (curr > end)
      Nil
    else
      curr::range(curr + 1, end)
  }

  def main(args: Array[String]): Unit = {
    println(range(4, 9))
  }
}