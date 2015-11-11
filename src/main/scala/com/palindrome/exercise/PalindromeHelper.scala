package com.palindrome.exercise

/**
 * Created by rambabu.posa on 11/2/2015.
 */
import java.util.Arrays

object PalindromeHelper {

  def findAndFormatTop3LongestPalindrome(inputStr: String): Unit = {
    findTop3LongestPalindrome(inputStr).foreach(p =>
      println("Text: " + p + " Index: " + inputStr.indexOf(p) + " Length: " + p.length))
  }

  def findTop3LongestPalindrome(inputStr:String) : List[String] = {

    val str1 = findLongestPalindrome(inputStr)

    val inputStr2 = inputStr.replaceAll(str1,"")
    val str2 = findLongestPalindrome(inputStr2)

    val inputStr3 = inputStr2.replaceAll(str2,"")
    val str3 = findLongestPalindrome(inputStr3)

    str1 :: str2 :: str3 :: Nil
  }

  def findLongestPalindrome(s: String): String = {
    if (s == null || s.length == 0) return ""
    val s2 = addBoundaries(s.toCharArray())
    val p = Array.ofDim[Int](s2.length)
    var c = 0
    var r = 0
    var m = 0
    var n = 0
    for (i <- 1 until s2.length) {
      if (i > r) {
        p(i) = 0
        m = i - 1
        n = i + 1
      } else {
        val i2 = c * 2 - i
        if (p(i2) < (r - i)) {
          p(i) = p(i2)
          m = -1
        } else {
          p(i) = r - i
          n = r + 1
          m = i * 2 - n
        }
      }
      while (m >= 0 && n < s2.length && s2(m) == s2(n)) {
        p(i) += 1
        m -= 1
        n += 1
      }
      if ((i + p(i)) > r) {
        c = i
        r = i + p(i)
      }
    }
    var len = 0
    c = 0
    for (i <- 1 until s2.length if len < p(i)) {
      len = p(i)
      c = i
    }
    val ss = Arrays.copyOfRange(s2, c - len, c + len + 1)
    String.valueOf(removeBoundaries(ss))
  }

  private def addBoundaries(cs: Array[Char]): Array[Char] = {
    if (cs == null || cs.length == 0) return "||".toCharArray()
    val cs2 = Array.ofDim[Char](cs.length * 2 + 1)
    var i = 0
    while (i < (cs2.length - 1)) {
      cs2(i) = '|'
      cs2(i + 1) = cs(i / 2)
      i = i + 2
    }
    cs2(cs2.length - 1) = '|'
    cs2
  }

  private def removeBoundaries(cs: Array[Char]): Array[Char] = {
    if (cs == null || cs.length < 3) return "".toCharArray()
    val cs2 = Array.ofDim[Char]((cs.length - 1) / 2)
    for (i <- 0 until cs2.length) {
      cs2(i) = cs(i * 2 + 1)
    }
    cs2
  }
}
