package fr.dboissin.s99.problems

/**
 * P91 : Another famous problem is this one:
 * How can a knight jump on an NÃ—N chessboard in such a way that
 * it visits every square exactly once?
 * 
 * @author dboissin
 */
object KnightTour {
  
  private val allowedMovement = List((1, -2), (-1, -2), (-2, -1), (2, -1),
	      (-2, 1), (2, 1), (-1, 2), (1, 2))
	      
  private def sortWarnsdorffs(path: List[(Int, Int)], size:Int) = {
    unorderedJumps(path, size).size
  }
  
  def jumps(path: List[(Int, Int)], size:Int) = {
	  unorderedJumps(path, size).sortWith((pos1, pos2) =>
	    sortWarnsdorffs(pos1::path, size) < sortWarnsdorffs(pos2::path, size))
  }
  
  def unorderedJumps(path: List[(Int, Int)], size:Int) = {
	val currentPosition = path.head
	allowedMovement.map(pos =>
	  (currentPosition._1 + pos._1, currentPosition._2 + pos._2)
	).filter { case (x, y) =>
	  x >= 0 && x < size && y >= 0 && y < size && !path.contains((x,y)) 
	}
  }
  
  def findPath(start: (Int, Int), size: Int) = {
    def find(path: List[(Int, Int)]): List[(Int, Int)] = 
      jumps(path, size) match {
        case Nil => path
        case head::tail => find(head::path)
      }
    find(start::Nil).reverse
  }

}
