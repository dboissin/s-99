package lists

/*
 Run-length encoding of a list.
 Use the result of problem P09 to implement the so-called run-length encoding data compression method.
 Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
*/
import P09.pack

object P10 {

  def encode(l: List[Symbol]): List[(Int, Symbol)] = {
     pack(l) map { e => (e.length, e.head)}
  }

  def main(args: Array[String]): Unit = {
    println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}