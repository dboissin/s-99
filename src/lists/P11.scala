package lists

/*
 RModified run-length encoding.
 Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list.
 Only elements with duplicates are transferred as (N, E) terms.
*/
import P09.pack

object P11 {

  def encode(l: List[Symbol]): List[Any] = {
     pack(l) map { e => if (e.length > 1) (e.length, e.head) else e.head}
  }

  def main(args: Array[String]): Unit = {
    println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}