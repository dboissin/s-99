package lists

/*
  Flatten a nested list structure.
 */

object P07 {

  def flatten(l: List[Any]): List[Any] = l flatMap {
    case ls: List[_] => flatten(ls)
    case el => List(el)
  }

  def main(args: Array[String]): Unit = {
    println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
  }
}