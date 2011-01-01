package lists

/*
 Lotto: Draw N different random numbers from the set 1..M.
 */

object P24 {
  import P23.randomSelect

  def lotto(nb: Int, size: Int): List[Int] =  {
    randomSelect(nb, (1 to size).toList)
  }

  def main(args: Array[String]): Unit = {
    println(lotto(6, 49))
  }
}