package fr.dboissin.s99.problems
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NQueensTest extends FunSuite{

  test("N-Queens - evaluation fonction") {
    val nb = 10
    val queens = NQueens(nb)
    assert(queens.evaluate == 45)
  }
  
  test("P90 : N-Queens - solve") {
//    val nb = 100
//    val queens = NQueens(nb, 1316472735258L)
    val nb = 8
    val queens = NQueens(nb, 1316472928555L)
    queens.solve
    println(queens)
    assert(queens.evaluate == 0)
  }
}