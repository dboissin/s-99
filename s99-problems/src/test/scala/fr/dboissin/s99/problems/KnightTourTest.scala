package fr.dboissin.s99.problems
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class KnightTourTest extends FunSuite {

  import fr.dboissin.s99.problems.KnightTour._
  
  test("Jump function") {
    val res = jumps(List((1, 0)), 8)
    assert(res == List((0, 2), (3, 1), (2, 2)))
  }
  
  test("P91 : Knight's tour - findPath") {
    val res = findPath((1,0), 8)
    assert(res.size == 64)
  }
}
