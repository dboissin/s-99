package lists

/*
 Run-length encoding of a list (direct solution).
 Implement the so-called run-length encoding data compression method directly.
 I.e. don't use other methods you've written (like P09's pack); do all the work directly.
*/

object P13  {

  def encodeDirect(l: List[Symbol]): List[Any] = l match {
    case Nil => Nil
    case _ => val tmp = l span(_ == l.head)
      (tmp._1.length, tmp._1.head)::encodeDirect(tmp._2)
  }

  def main(args: Array[String]): Unit = {
    println(encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}