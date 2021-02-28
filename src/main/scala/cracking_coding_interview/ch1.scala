package cracking_coding_interview

object ch1 extends App {

  /**
   * Question 1.1
   * O(n) time and O(n) space
   */
  def isUnique(string: String): Boolean = {
    val charArray = string.toCharArray
    var hashMap = Map[Char, Int]()
    charArray.foreach(char => {
      if(hashMap.contains(char))
        return false
      else
        hashMap += char -> 1
    })
    true
  }

  /**
   * Question 1.2
   * Uses a hashmap of character to count
   * If these are the same then they are permutations of each other
   * O(n) time and O(n) space
   *
   * Could also sort the 2 lists and then compare the resulting strings
   * this would be O(nlogn)
   */
  def permutation(string1: String, string2: String): Boolean = {
    def stringToMap(string: String): Map[Char, Int] = {
      var hashMap = Map[Char, Int]()
      string.toCharArray.foreach(char => {
        val oldValue: Int = hashMap.getOrElse(char, 0)
        val newValue: Int = oldValue +1
        hashMap += (char -> newValue)
      })
      hashMap
    }
    val mapString1 = stringToMap(string1)
    val mapString2 = stringToMap(string2)
    if(mapString1 equals mapString2) true
    else false
  }

  /**
   * Q 1.3
   * Uses of fold left to fold over the string, accumulating the results in another string
   */
  def urlify(string: String): String = {
    string.toCharArray.foldLeft("")((acc, char) => {
      if(char.toString == " ")
        acc ++ "%20"
      else acc ++ char.toString
    })
  }

  /**
   * Q 1.4
   * Check if a string is a permutation of a palindrome
   * Recursive but not tail recursive
   */
  def palindromePermutation(string: String): Boolean = {
    def isPalindrome(string: String): Boolean =
      if(string.reverse == string) true else false

    def permutations(s: String): List[String] = {
      def merge(string: String, c: Char): Seq[String] =
        for (i <- 0 to string.length) yield
          string.substring(0, i) + c + string.substring(i, string.length)

      if (s.length() == 1)
        List(s)
      else
        permutations(s.substring(0, s.length - 1)).flatMap { p =>
          merge(p, s.charAt(s.length - 1))
        } 
    }

    if(permutations(string).exists(isPalindrome)) true else false
    }


  /**
   * Q1.5
   * Edit distance between two strings, if <= 1 then true else false
   * String can have
   * 1. More characters
   * 2. less characters
   * 3. Different characters
   */
  def oneAway(string1: String, string2: String): Boolean = {
    var editCount = 0;
    var i = 0
    var j = 0;
    while (i < string1.length && j < string2.length)
    {
      // If current characters don't match
      if (string1.charAt(i) != string2.charAt(j))
      {
        if (editCount == 1)
          return false;

        // If length of one string is more, then only possible edit is to remove a character
        if (string1.length > string2.length)
          i+=1
        else if (string2.length < string1.length)
          j+=1
        else i+=1; j+=1

        editCount+=1
      }
      // If current characters match
      else i+=1; j+=1
    }
    if (i < string1.length  || j < string2.length)
      editCount+=1

    editCount == 1
  }

  /**
   * Q 1.6
   */
  def stringCompression(string: String): String = {
    val stringChars: Array[Char] = string.toCharArray
    var stringBuilder = ""
    for(i <- 0 until string.length-1){
      val character = stringChars(i)
      val counter = 1;
      while(stringChars(i) == stringChars(i+1)) {
        counter+1
      }
      stringBuilder+= character.toString ++ counter.toString
    }
    stringBuilder
  }


  /**
   * 4.1
   * Route between two nodes
   */
  case class Node(visited: Boolean, adj: List[Node])
  def routeBetweenNode(node1: Node, node2: Node): Boolean = {

    def inner(currentNode: Node, targetNode: Node): Boolean = {
      if(!currentNode.visited){
        for(node <- currentNode.adj) {
          if(node == targetNode){
            return true
          }
        }
      }
      false
    }
    inner(node1, node2)
  }
}
