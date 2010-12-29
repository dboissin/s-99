package lists

/*
 Drop every Nth element from a list.
 */

object P16 {

  def drop(n: Int, l: List[Symbol]): List[Symbol] = {
    def dropTmp(curN: Int, curL: List[Symbol]): List[Symbol] = curL match {
      case Nil => Nil
      case _ => if (curN == 1) dropTmp(n, curL.tail) else curL.head::dropTmp(curN-1, curL.tail)
    }
    dropTmp(n, l)
  }

  def main(args: Array[String]): Unit = {
    println(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

}