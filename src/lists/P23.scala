package lists

/*
  Extract a given number of randomly selected elements from a list.
 */

object P23 {

  import P20.removeAt
  import java.util.Random

  def randomSelect[T](nb: Int, l: List[T]): List[T] =  {
    if (l == Nil)
      return Nil
    val (ls, el) = removeAt(new Random().nextInt(l.length), l)
    if (nb < 1)
      Nil
    else
      el::randomSelect(nb-1, ls)
  }

  def main(args: Array[String]): Unit = {
    println(randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))
  }
}