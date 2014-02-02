package com.verhein.florian.plala

/** Sort a string in order that the characters appear in another string */
object Exercise_8 {

  //assume by has no duplicate characters (otherwise ordering ill-defined)

  def sortBy1(str : String, by : String) = {

    case class OrderingByMap[T](by : Map[T,Int]) extends Ordering[T]{
      def compare(x : T, y : T) = by(x) - by(y)
    }

    val o = new OrderingByMap(by.zipWithIndex.toMap)
    str.sorted(o)
  }

  def sortBy2(str: String, by : String) = {
    val o = by.zipWithIndex.toMap
    str.sortBy(c => o(c))
  }

}
