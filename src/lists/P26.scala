package lists

object P26 {

	def combinations2[T](l: List[T]): List[List[T]] = {
			if (l == Nil)
				return Nil
			val tmp = l.tail map (el => List(l.head, el))
			tmp:::combinations2(l.tail)
	}

	def combinations[T](nb: Int, l: List[T]): List[List[Any]] = {
		def lift[A](xs:List[A]):List[List[A]]=xs.foldLeft(List[List[A]]())((ys,y)=>(List(y)::ys))
		println(lift(l))
		(nb, l) match {
		case (1, ls) => lift(ls)
		case (_, Nil) => Nil
		case (_, ls) => combinations(nb-1, ls.tail).map(el => ls.head::el):::combinations(nb, l.tail)
	}}

	def main(args: Array[String]): Unit = { 
			println(combinations2(List('a, 'b, 'c, 'd, 'e, 'f)))
			println(combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)))
	}

}