package fr.dboissin.s99.problems

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

/**
 * 
 * @author dboissin
 */
@RunWith(classOf[JUnitRunner])
class S99ListTest extends FunSuite {

  import fr.dboissin.s99.problems.S99List._

  test("P01 : Find the last element of a list.") {
    val res = last(List(1, 1, 2, 3, 5, 8))
    assert(res == 8)
  }

  test("P02 : Find the last but one element of a list.") {
    val res = penultimate(List(1, 1, 2, 3, 5, 8))
    assert(res == 5)
  }

  test("P03 : Find the Kth element of a list.") {
    val res = nth(2, List(1, 1, 2, 3, 5, 8))
    assert(res == 2)
  }
  
  test("P04 : Find the number of elements of a list.") {
    val res = length(List(1, 1, 2, 3, 5, 8))
    assert(res == 6)
  }
  
  test("P05 : Reverse a list.") {
    val res = reverse(List(1, 1, 2, 3, 5, 8))
    assert(res == List(8, 5, 3, 2, 1, 1))
  }
  
  test("P06 : Find out whether a list is a palindrome.") {
    val res = isPalindrome(List(1, 2, 3, 2, 1))
    assert(res == true)
  }
  
  test("P07 : Flatten a nested list structure.") {
    val res = flatten(List(List(1, 1), 2, List(3, List(5, 8))))
    assert(res == List(1, 1, 2, 3, 5, 8))
  }
  
  test("P08 : Eliminate consecutive duplicates of list elements.") {
    val res = compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assert(res == List('a, 'b, 'c, 'a, 'd, 'e))
  }
  
  test("P09 : Pack consecutive duplicates of list elements into sublists.") {
    val res = pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    val expected = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c),
               List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    assert(res == expected)
  }
  
  test("P10 : Run-length encoding of a list.") {
    val res = encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assert(res == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }
  
  test("P11 : Modified run-length encoding.") {
    val res = encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assert(res == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
  }
  
  test("P12 : Decode a run-length encoded list.") {
    val res = decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    assert(res == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }
  
  test("P13 : Run-length encoding of a list (direct solution).") {
     val res = encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
     assert(res == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  test("P14 : Duplicate the elements of a list.") {
    val res = duplicate(List('a, 'b, 'c, 'c, 'd))
    assert(res == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  test("P15 : Duplicate the elements of a list a given number of times.") {
    val res = duplicateN(3, List('a, 'b, 'c, 'c, 'd))
    assert(res == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }
  
  test("P16 : Drop every Nth element from a list.") {
    val res = drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(res == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }
  
  test("P17 : Split a list into two parts.") {
    val res = split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(res == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }
  
  test("P18 : Extract a slice from a list.") {
    val res = slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(res == List('d, 'e, 'f, 'g))
  }
  
  test("P19 : Rotate a list N places to the left.") {
    val res = rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(res == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))

    val res2 = rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(res2 == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }
  
  test("P20 : Remove the Kth element from a list.") {
    val res = removeAt(1, List('a, 'b, 'c, 'd))
    assert(res == (List('a, 'c, 'd),'b))
  }
  
  test("P21 : Insert an element at a given position into a list.") {
    val res = insertAt('new, 1, List('a, 'b, 'c, 'd))
    assert(res == List('a, 'new, 'b, 'c, 'd))
  }
  
  test("P22 : Create a list containing all integers within a given range.") {
    val res = range(4, 9)
    assert(res == List(4, 5, 6, 7, 8, 9))
  }
  
  test("P23 : Extract a given number of randomly selected elements from a list.") {
    val res = randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
    assert(res.size == 3)
  }
  
  test("P24 : Lotto: Draw N different random numbers from the set 1..M.") {
    val res = lotto(6, 49)
    assert(res.size == 6)
  }
  
  test("P25 : Generate a random permutation of the elements of a list.") {
    val l = List('a, 'b, 'c, 'd, 'e, 'f)
    val res = randomPermute(l)
    assert(!res.sameElements(l))
    assert(res.diff(l).size == 0)
  }
  
}
