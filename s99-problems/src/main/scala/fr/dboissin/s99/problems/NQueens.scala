package fr.dboissin.s99.problems
import java.util.Date
import scala.util.Random
import scala.math._

/**
 * P90 : Eight queens problem
 * This is a classical problem in computer science. 
 * The objective is to place eight queens on a chessboard 
 * so that no two queens are attacking each other;
 * i.e., no two queens are in the same row, the same column, or on the same diagonal.
 * 
 * @author dboissin
 */
case class NQueens (
    val size: Int,
    val seed: Long = new Date().getTime 
) {
  
  val random: Random = new Random(seed)
  var queens = 0 to size-1 toArray
  
  def evaluate() = {
    var nbr = 0
	for (i <- 0 until queens.size)
	  for (j <- (i + 1) until queens.size)
	    if (abs(queens.apply(j) - queens.apply(i)) == abs(j - i))
	      nbr += 1
	nbr
  }
  
  def swap(i: Int, j: Int) {
    val tmp = queens.apply(i)
    queens.update(i, queens.apply(j))
    queens.update(j, tmp)
  }
  
  def solve() {
    var conflicts = 999999999
    while (conflicts != 0) {
      val i = abs(random.nextInt()%size)
      val j = abs(random.nextInt()%size)
      swap(i, j)
      val c = evaluate()
      if (c < conflicts) {
        conflicts = c
      } else {
        swap(j, i)
      }
    }
  }
  
  override def toString = "Seed : " + seed + " - Solution : " + queens.toList
}
