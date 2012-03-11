package fr.dboissin.s99.problems

case class Cell(x: Int, y: Int, coeff: Int = 1) {
  def neighbors = List(
  Cell(x - 1, y - 1), Cell(x - 1, y), Cell(x - 1, y + 1),
  Cell(x + 1, y - 1), Cell(x + 1, y), Cell(x + 1, y + 1),
  Cell(x, y - 1), Cell(x, y + 1)
  )
}

case class Universe(lifeCells: List[Cell]) {
  def next = {
    val potentials = lifeCells.flatMap(c => c :: c.neighbors)./:[List[Cell]](Nil){(acc, c) =>
      acc.partition(cell => cell.x == c.x && cell.y == c.y) match {
        case (Nil, _) => c :: acc
        case (el, l) => Cell(c.x, c.y, el.head.coeff + c.coeff) :: l
      }
    }
    Universe(potentials./:[List[Cell]](Nil)((acc, c) =>
      if (c.coeff == 3 || c.coeff == 11 || c.coeff == 12) c.copy(coeff = 9) :: acc
      else acc))
  }
}

object Universe {
  def apply(): Universe = {
    Universe((1 to 10).toList.map(i => Cell(i, i, 9)))
  }
}

