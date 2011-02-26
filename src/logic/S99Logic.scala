package logic

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

	class S99Logic (a: Boolean) {
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
}