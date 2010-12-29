package lists

/*
  Split a list into two parts.
  The length of the first part is given. Use a Tuple for your result.
 */

object P17 {

  def split(n: Int, l: List[Symbol]): (List[Symbol],List[Symbol]) =
    l splitAt n

  def main(args: Array[String]): Unit = {
    println(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }
}