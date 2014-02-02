package com.verhein.florian.plala

//TODO improve this: don't expand paths that will mean more flips later (i.e. use the sum idea as an optimisation function)

/** a) Determine whether a given string of parentheses is balanced.
  * b) Determine the minimum number of flips required to make a string of parentheses balanced (and what they are)
  */
object Exercise_5 {

  def isBalanced(s : String, i : Int = 0, depthSoFar : Int = 0) : Boolean = {
    if (i == s.length)
      depthSoFar == 0
    else {
      val nd = s(i) match {
        case '(' => depthSoFar+1
        case ')' => depthSoFar-1
        case default => depthSoFar
      }
      if (nd < 0)
        false
      else
        isBalanced(s,i+1,nd)
    }
  }

  case class State(prev : Option[State], depth : Int,  flipped : Int, s : String) {
    override def toString = (depth,flipped,"#"+s+"#").toString
  }

  implicit val stateOrder = Ordering.by[State,Int](_.depth).reverse

  def path(st : State) : List[Int] = 
    st.prev match {
      case Some(s) => st.flipped :: path(s)
      case None => Nil
    }

  def goal(st : State) = isBalanced(st.s)

  def findFlips(s : String) = {

    val seen = collection.mutable.Set[String]()

    def replace(s : String, i : Int, c : Character) = {
      val suf = if (i == s.length-1) "" else s.substring(i+1,s.length)
      s.substring(0,i) + c + suf
    }

    def newState(st : State, i: Int) = {
      val nc = st.s(i) match {
        case '(' => ')'
        case ')' => '('
        case default => throw new Exception("Bad character @" + i + " of " + st.s)
      }
      State(Some(st),st.depth+1,i,replace(st.s,i,nc))
    }

    def expand(st : State, from : Int = 0) : List[State] = {
      if (from < st.s.length) {
        val ns = newState(st, from)
        if (seen(ns.s)) {
          expand(st,from+1)
        }
        else {
          seen += ns.s
          ns :: expand(st,from+1)
        }
      }
      else Nil 
   }

    val queue = collection.mutable.PriorityQueue[State]()

    def search() : Option[State] = {
      if (queue.isEmpty)
        None
      else
      {
        //println(queue)
        val st = queue.dequeue
        //println(st)
        if (goal(st)) {
          //println("goal!")
          Some(st)
        }
        else {
          queue ++= expand(st)
          search
        }
      }
    }

    val initial = State(None,0,-1,s)
    queue += initial
    val found = search
    //println(found)
    found.map(s => path(s))
  }
  
  def main(args : Array[String]) : Unit = {
    val s = args(0)
    println(isBalanced(s))
    println(findFlips(s))
    Unit
  }
}


