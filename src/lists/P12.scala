package lists

/*
Decode a run-length encoded list.
Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
*/


object P12 {

  def decode(l: List[(Int, Symbol)]): List[Symbol] =
    l flatMap { e => List.fill(e._1)(e._2)}

  def main(args: Array[String]): Unit = {
    println(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))
  }
}