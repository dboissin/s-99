package fr.dboissin.s99.problems

/**
 *
 * @author dboissin
 */
object S99Logic {

	implicit def boolean2S99Logic(a: Boolean): S99Logic = new S99Logic(a)

	def and(a:Boolean, b: Boolean): Boolean = (a, b) match {
	case (true, true) => true
	case _ => false
	}

	def or(a:Boolean, b: Boolean): Boolean = (a, b) match {
	case (false, false) => false
	case _ => true
	}

	def nand(a:Boolean, b: Boolean): Boolean = !and(a,b)

	def nor(a:Boolean, b: Boolean): Boolean = !or(a,b)

	def xor(a: Boolean, b: Boolean): Boolean = (a, b) match {
	case (false, true) => true
	case (true, false) => true
	case _ => false
	}

	def impl(a:Boolean, b: Boolean): Boolean = or(!a,b)

	def equ(a:Boolean, b: Boolean): Boolean = !xor(a,b)

	def table2(f: (Boolean, Boolean) => Boolean) = {
		println("A        B       Res")
		for (a <- List(false, true);b <- List(false, true)) {
			println(a + " " + b + " " + f(a, b))
		}
	}

	def grey(nb: Int): List[String] = nb match {
		case 1 => List("0", "1")
		case _ => val tmp = grey(nb - 1)
		(tmp map (value => "0" + value)):::(tmp.reverse map (value => "1" + value))
	}

	def huffman[T](symbols: List[(T,Int)]):List[(T, String)] = {
		import scala.collection.mutable._

		implicit val ordering = new Ordering[Tree[T]] {
			def compare(o1:Tree[T], o2:Tree[T]): Int = {
				o2.freq.compare(o1.freq)
			}
		}
		val queue = new PriorityQueue[Tree[T]]();
		symbols foreach { case (value, freq) => (queue += Leaf(value, freq))}

		for (i <- 2 to symbols.size) {
			val tmp1 = queue.dequeue
			val tmp2 = queue.dequeue
			queue += Node(tmp1.freq + tmp2.freq, tmp1, tmp2)
		}
		queue.dequeue.toPrefix("")
	}

	private abstract class Tree[T] {
		val freq: Int;
		def toPrefix(prefix: String):List[(T, String)]
	}

	private case class Node[T](freq: Int, left:Tree[T], right:Tree[T]) extends Tree[T] {
		def toPrefix(prefix: String) = {
			left.toPrefix(prefix + "0"):::right.toPrefix(prefix + "1")
		}
	}

	private case class Leaf[T](value:T, freq: Int) extends Tree[T] {
		def toPrefix(prefix:String) = List((value, prefix))
	}

}

class S99Logic (a: Boolean) {
	import S99Logic._

	def and(b:Boolean): Boolean = (a, b) match {
	case (true, true) => true
	case _ => false
	}

	def or(b: Boolean): Boolean = (a, b) match {
	case (false, false) => false
	case _ => true
	}

	def nand(b: Boolean): Boolean = !(a and b)

	def nor(b: Boolean): Boolean = !(a or b)

	def xor(b: Boolean): Boolean = (a, b) match {
	case (false, true) => true
	case (true, false) => true
	case _ => false
	}

	def impl(b: Boolean): Boolean = !a or b

	def equ(a:Boolean, b: Boolean): Boolean = !(a xor b)
}
