package fr.dboissin.s99.problems

	sealed abstract class Tree[+T]

	case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
		override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
	}

	case object End extends Tree[Nothing] {
		override def toString = "."
	}
	
	object Node {
		def apply[T](value: T): Node[T] = Node(value, End, End)
	}
	
//	object Tree {
//	
//		def cBalanced[T](nodes: Int, value:T): List[Tree[T]] = {
//	
//		}
//	}
