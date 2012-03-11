package fr.dboissin.s99.problems

import org.scalatest.FunSuite

class ConwayTest extends FunSuite {

  test("Conway life : Generate next univers") {
    val universe = Universe()
    val res = universe.next
    println(universe)
    println(res)
    assert(res.lifeCells.size > 0)
  }

}

