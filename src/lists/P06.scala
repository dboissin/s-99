package lists

/*
 Find out whether a list is a palindrome.
 */

object P06 {

  def isPalindrome[T](l: List[T]) =
    l.reverse == l

  def main(args: Array[String]): Unit = {
    println(isPalindrome(List(1, 2, 3, 2, 1)))
    println(isPalindrome(List(1, 2, 3, 4, 5)))
  }
}