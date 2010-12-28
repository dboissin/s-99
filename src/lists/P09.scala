package lists

/*
  Pack consecutive duplicates of list elements into sublists.
  If a list contains repeated elements they should be placed in separate sublists.
 */

object P09 {

  def pack(l: List[Symbol]): List[List[Symbol]] = l match {
    case Nil => Nil
    case _ => l.takeWhile(_ == l.head)::pack(l.dropWhile(_ == l.head))

  }

  def main(args: Array[String]): Unit = {
    println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}