package lists

/*
    Find the last element of a list.
 */

object P01 {
    def last[T](l:List[T]):T = l match {
        case x::Nil => x
        case _::xs => last(xs)
    case Nil => throw new NoSuchElementException
    }

    def main(args : Array[String]) : Unit = {
        println(last(List(1, 1, 2, 3, 5, 8)))
    }
}