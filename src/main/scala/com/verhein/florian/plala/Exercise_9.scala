package com.verhein.florian.plala


/**
  * For an array of integers, give an algorithm to determine if there
  * are three elements that sum to zero. 
  * What are the time and space complexity? 
  * Generalize to the case where the sum of k elements is 0 */
object Exercise_9 {

  def sumOfk(k : Int, data : Array[Int]) = {

    case class State(i : Int, depth : Int, sum : Int)

    def search(k : Int)(s : State) : Option[List[Int]] = {
      if (s.depth == k) {
        if (s.sum == 0) Some(s.i :: Nil)
        else None
      } else {
        for (i <- s.i+1 until data.length) {
          val ns = State(i,s.depth+1, s.sum + data(i))
          search(k)(ns) map {
            case l => return Some(s.i :: l)
          }
        }
        return None
      }
    }

    search(k)(State(-1,0,0)).map(_.tail)
  }

  def sumOfK2(k : Int, data : Array[Int]) = {
    //Pruning:
    // - if all > 0 or all < 0 then no solution
    // - any numbers > abs(sumK(data < 0))

    //then somOfK
  }



}
