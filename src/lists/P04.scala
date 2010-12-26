package lists

/**
  Find the number of elements of a list.
 */

object P04 {

  def length[T](l: List[T]) =
    l.foldLeft(0)((count, _) => (count + 1))

  def main(args: Array[String]): Unit = {
    println(length(List(1, 1, 2, 3, 5, 8)))
  }
}