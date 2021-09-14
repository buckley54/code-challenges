package com.intenthq.challenge

import scala.annotation.tailrec;

object SEnigma {

  // We have a system to transfer information from one place to another. This system
  // involves transferring only list of digits greater than 0 (1-9). In order to decipher
  // the message encoded in the list you need to have a dictionary that will allow
  // you to do it following a set of rules:
  //    > Sample incoming message: (1,2,3,7,3,2,3,7,2,3,4,8,9,7,8)
  //    > Sample dictionary (23->‘N’,234->‘ ’,89->‘H’,78->‘Q’,37 ->‘A’)
  //  - Iterating from left to right, we try to match sublists to entries of the map.
  //    A sublist is a sequence of one or more contiguous entries in the original list,
  //    eg. the sublist (1, 2) would match an entry with key 12, while the sublist (3, 2, 3)
  //    would match an entry with key 323.
  //  - Whenever a sublist matches an entry of the map, it’s replaced by the entry value.
  //    When that happens, the sublist is consumed, meaning that its elements can’t be used
  //    for another match. The elements of the mapping however, can be used as many times as needed.
  //  - If there are two possible sublist matches, starting at the same point, the longest one
  //    has priority, eg 234 would have priority over 23.
  //  - If a digit does not belong to any matching sublist, it’s output as is.
  //
  // Following the above rules, the message would be: “1N73N7 HQ”
  // Check the tests for some other (simpler) examples.

  def deciphe(map: Map[Int, Char])(message: List[Int]): String = {
    val nMap = map.map(kv => kv._1.toString -> kv._2)
    val messString = message.mkString
    @tailrec
    def decode(original: String, current: String, res: String = ""): String = {
      original.length match {
        case 0 => res
        case _ if nMap.contains(current) =>
          val rest = original.drop(current.length)
          decode(rest, rest, res + nMap(current))
        case _ if current.length == 1 =>
          val rest = original.drop(1)
          decode(rest, rest, res + current)
        case _ => decode(original, current.dropRight(1), res)
      }
    }

    decode(messString, messString)
  }

}