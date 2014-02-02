package com.verhein.florian.plala.exercise4

/**
  * Given a bar graph, how much water could it hold.
  * O(n) solution.
  */
object Exercise_4 {

  def lastMax(heights : Array[Double]) = {
    val (_, maxIndex,maxHeight) = heights.foldLeft[(Int,Int,Double)]((-1,-1,0.0)){
      case ((i,mi,m),h) =>
        if (h >= m)
          (i+1,i+1,h)
        else
          (i+1,mi,m)
    }
    (maxIndex,maxHeight)
  }

  def volumeHeld(heights : Array[Double], width : Double = 1.0) = {

    case class State(i : Int, vol : Double, max : Double)

    val (maxIndex,maxHeight) = lastMax(heights)

    val fromLeft = heights.foldLeft[State](new State(-1,0,0)){
      case (s,h) => 
        if (s.i < maxIndex)
          State(s.i+1, s.vol + math.max(s.max-h,0)*width, math.max(s.max,h))
        else s
    }

    val completed = heights.foldRight[State](new State(heights.length, fromLeft.vol, 0)){
      case (h,s) =>
        if (s.i > maxIndex)
          State(s.i-1, s.vol + math.max(s.max-h,0)*width, math.max(s.max,h))
        else s
    }

    completed.vol
  }
}

