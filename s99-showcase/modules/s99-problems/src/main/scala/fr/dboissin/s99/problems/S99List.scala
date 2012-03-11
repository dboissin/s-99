package fr.dboissin.s99.problems

/**
 *
 * @author dboissin
 */
object S99List {

  /**
   * P01 : Find the last element of a list.
   */
  def last[T](l:List[T]):T = l match {
    case x::Nil => x
    case _::xs => last(xs)
    case Nil => throw new NoSuchElementException
  }

  /**
   * P02 : Find the last but one element of a list.
   */
  def penultimate[T](l: List[T]) : T = l match {
    case head::_::Nil => head
    case _::tail => penultimate(tail)
    case Nil => throw new NoSuchElementException
  }

  /**
   * P03 : Find the Kth element of a list.
   */
  def nth[T](n: Int, l: List[T]): T = l match {
    case head::tail => if (n == 0) head else nth(n-1, tail)
    case _ => throw new NoSuchElementException
  }

  /**
   * P04 : Find the number of elements of a list.
   */
  def length[T](l: List[T]) =
    l.foldLeft(0)((count, _) => (count + 1))

  /**
   * P05 : Reverse a list.
   */
  def reverse[T](l: List[T]): List[T] = l match {
    case head::Nil => head::Nil
    case head::tail => reverse(tail):::List(head)
    case _ => throw new NoSuchElementException
  }

  /**
   * P06 : Find out whether a list is a palindrome.
   */
  def isPalindrome[T](l: List[T]) =
    l.reverse == l

  /**
   * P07 : Flatten a nested list structure.
   */
  def flatten(l: List[Any]): List[Any] = l flatMap {
    case ls: List[_] => flatten(ls)
    case el => List(el)
  }

  /**
   * P08 : Eliminate consecutive duplicates of list elements.
   * If a list contains repeated elements they should be replaced with a single
   * copy of the element. The order of the elements should not be changed.
   */
  def compress(l: List[Symbol]): List[Symbol] = l match {
    case Nil => Nil
    case head::tail => head::compress(tail.dropWhile(_ == head))
  }

  /**
   * P09 : Pack consecutive duplicates of list elements into sublists.
   * If a list contains repeated elements they should be placed in separate sublists.
   */
  def pack(l: List[Symbol]): List[List[Symbol]] = l match {
    case Nil => Nil
    case _ => l.takeWhile(_ == l.head)::pack(l.dropWhile(_ == l.head))
  }

  /**
   * P10 : Run-length encoding of a list.
   * Use the result of problem P09 to implement the so-called run-length
   * encoding data compression method. Consecutive duplicates of elements are
   * encoded as tuples (N, E) where N is the number of duplicates of the element E.
   */
  def encode(l: List[Symbol]): List[(Int, Symbol)] = {
     pack(l) map { e => (e.length, e.head)}
  }

  /**
   * P11 : Modified run-length encoding.
   * Modify the result of problem P10 in such a way that if an element has no duplicates
   * it is simply copied into the result list. Only elements with duplicates
   * are transferred as (N, E) terms.
   */
  def encodeModified(l: List[Symbol]): List[Any] = {
     pack(l) map { e => if (e.length > 1) (e.length, e.head) else e.head}
  }

  /**
   * P12 : Decode a run-length encoded list.
   * Given a run-length code list generated as specified in problem P10,
   * construct its uncompressed version.
   */
  def decode(l: List[(Int, Symbol)]): List[Symbol] =
    l flatMap { e => List.fill(e._1)(e._2)}

  /**
   * P13 : Run-length encoding of a list (direct solution).
   * Implement the so-called run-length encoding data compression method directly.
   * I.e. don't use other methods you've written (like P09's pack); do all the work directly.
   */
  def encodeDirect(l: List[Symbol]): List[Any] = l match {
    case Nil => Nil
    case _ => val tmp = l span(_ == l.head)
      (tmp._1.length, tmp._1.head)::encodeDirect(tmp._2)
  }

  /**
   * P14 : Duplicate the elements of a list.
   */
  def duplicate(l: List[Symbol]): List[Symbol] =
    l flatMap (el => List(el, el))

  /**
   * P15 : Duplicate the elements of a list a given number of times.
   */
  def duplicateN(nb: Int, l: List[Symbol]): List[Symbol] =
    l flatMap (el => List.fill(nb)(el))

  /**
   * P16 : Drop every Nth element from a list.
   */
  def drop(n: Int, l: List[Symbol]): List[Symbol] = {
    def dropTmp(curN: Int, curL: List[Symbol]): List[Symbol] = curL match {
      case Nil => Nil
      case _ => if (curN == 1) dropTmp(n, curL.tail) else curL.head::dropTmp(curN-1, curL.tail)
    }
    dropTmp(n, l)
  }

  /**
   * P17 : Split a list into two parts.
   * The length of the first part is given. Use a Tuple for your result.
   */
  def split(n: Int, l: List[Symbol]): (List[Symbol],List[Symbol]) =
    l splitAt n

  /**
   * P18 : Extract a slice from a list.
   * Given two indices, I and K, the slice is the list containing the elements from
   * and including the Ith element up to but not including the Kth element of the original list.
   * Start counting the elements with 0.
   */
  def slice[T](i: Int, k: Int, l: List[T]): List[T] = (i, k, l) match {
    case (_, _, Nil) => Nil
    case (_, 1, _) => l.head::Nil
    case (0, _, _) => l.head::slice(0, k-1, l.tail)
    case (_, _, _) => slice(i-1, k-1, l.tail)
  }

  /**
   * P19 : Rotate a list N places to the left.
   */
  def rotate[T](offset: Int, l: List[T]): List[T] = {
    val (l1, l2) = l.splitAt(if (offset < 0) l.length + offset else offset)
    l2:::l1
  }

  /**
   * P20 : Remove the Kth element from a list.
   * Return the list and the removed element in a Tuple. Elements are numbered from 0.
   */
  def removeAt[T](n: Int, l: List[T]): (List[T], T) = (n, l) match {
    case (_, Nil) => throw new NoSuchElementException
    case (0, _) => (l.tail, l.head)
    case (_, _) => val (ls,el) = removeAt(n-1, l.tail)
      (l.head::ls, el)
  }

  /**
   * P21 : Insert an element at a given position into a list.
   */
  def insertAt[T](el: T, n: Int, l: List[T]): List[T] = (n,l) match {
    case (_, Nil) => Nil
    case (0, _) => el::l
    case (_, _) => l.head::insertAt (el, n-1, l.tail)
  }

  /**
   * P22 : Create a list containing all integers within a given range.
   */
  def range(curr: Int, end: Int): List[Int] = {
    if (curr > end)
      Nil
    else
      curr::range(curr + 1, end)
  }

  /**
   * P23 : Extract a given number of randomly selected elements from a list.
   */
  def randomSelect[T](nb: Int, l: List[T]): List[T] =  {
    import java.util.Random

    if (l == Nil)
      return Nil
    val (ls, el) = removeAt(new Random().nextInt(l.length), l)
    if (nb < 1)
      Nil
    else
      el::randomSelect(nb-1, ls)
  }

  /**
   * P24 : Lotto: Draw N different random numbers from the set 1..M.
   */
  def lotto(nb: Int, size: Int): List[Int] =  {
    randomSelect(nb, (1 to size).toList)
  }

  /**
   * P25 : Generate a random permutation of the elements of a list.
   */
  def randomPermute[T](l: List[T]): List[T] =
    randomSelect(l.length, l)

}
