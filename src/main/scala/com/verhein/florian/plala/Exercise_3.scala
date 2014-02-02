package com.verhein.florian.plala.exercise3

import scala.collection._

/**
  * Given two arrays x and y, find the kth min((x_i,y_j)).
  * Assume x and y sorted. 
  * 
  * Optimal O(k) solution
  *
  * Example: M[i,j] = min(x[i],y[j]) 
  * M = 
  *    1 2 3 4 5 9 10 20 30 x 
  * 2  1 2 2 2 2 2 y  y  y
  * 3  1 2 3 3 3 3 y  .  .
  * 5  1 2 3 4 5 5 y 
  * 7  1 2 3 4 5 7 y
  * 8  1 2 3 4 5 8 y
  * 9  1 2 3 4 5 9 y  .
  * 10 x x x x x x e  y  y
  * 11 x x x x x x x  y  y
  * 12 x x x x x x x  y  y
  * y
  * 
  */
object Exercise_3v1 {

  def pairwiseKthMin[A](x : Array[A], y : Array[A], k : Int)(implicit ord : Ordering[A]) : Option[A] = {

    import math._

    sealed abstract class Move
    case object X extends Move
    case object Y extends Move
    case object B extends Move

    // c = count so far
    def find(i : Int, j : Int, c : Int) : A = {

      println((i,j,c))

      val res = (x(i),y(j)) match {
        case (xi,yj) if (ord.lt(xi,yj)) =>
          //M[i, >= j] == xi
          (c + y.length - j, xi, X)
        case (xi,yj) if (ord.gt(xi,yj)) =>
          //M[ >= i, j] = yj
          (c + x.length - i, yj, Y)
        case (xi,yj) =>
          //M[ >= i, >= j] = xi = yj
          (c + x.length - i + y.length - j, xi, B)
      }

      println(res)

      // M will have value=vaue for newC - c steps
      res match {
        case (newC,value,move) if (newC >= k) =>
          value  // Found solution
        case (newC,value,move) =>
          move match {
            case X => find(i+1,j,newC)
            case Y => find(i,j+1,newC)
            case B => find(i+1,j+1,newC)
          }
      }
    }
    if (k > x.length * y.length) None
    else Some(find(0,0,0))
  }

  def main(args : Array[String]) : Unit = {

    val rand = new util.Random(args(0).toLong)
    def gen(n : Int) = Range(0,n).foldLeft[List[Int]](Nil)((l,_) => rand.nextInt :: l).toArray.sorted

    val x = gen(args(0).toInt)
    val y = gen(args(1).toInt)

    println(x.toList)
    println(y.toList)
    println(pairwiseKthMin(x,y,args(2).toInt))
  }
}


object Exercise_3v0 {

  /** Assume x,y sorted increasing, no duplicates exist.
    * If duplicates exist, this just duplicates results so does not alter the underlying algorithm.
    * 
    * Can there be more than one path? yes, generally a tree structure
    * 
    * M = 
    *    1 2 3 4 5 9 10 20 30 x 
    * 2  1 2 2 2 2 2 y  y  y
    * 3  1 2 3 3 3 3 y  .  .
    * 5  1 2 3 4 5 5 y 
    * 7  1 2 3 4 5 7 y
    * 8  1 2 3 4 5 8 y
    * 9  1 2 3 4 5 9 y  .
    * 10 x x x x x x e  y  y
    * 11 x x x x x x x  y  y
    * 12 x x x x x x x  y  y
    * y
    * 
    */
  
/*

  case class State[A](i : Int, j : Int, v : A) //Note: no point putting depth of search here as will = i+j


  def pairwiseKthMin[A](x : Array[A], y : Array[A], k : Int)(implicit ord : Ordering[A]) {

    import math._

    def eval(s : State) = min(x[s.i],y[s.j])

    def inc(x : Array[A], i : Int) = {
      x[i+1] - x[i]
    }

    def incObj(x : Array[A], i : Int) = {
      x[i+1] - x[i]
    }



    def newState(i : Int, j : Int) = State(i,j,eval(i,j))

    def expand(s : State) : List[State] = {
      //Choose direction. Can only move down or right as moving down+right is an upper bound on either
      //Don't return both as the cell we don't return can always be reached via another path.
      //E.g. if we go right, then the cell below can be reached via a previous branch -- TODO prve that this is always the case

      val incX = x(i+1) - x(i)
      val incY = y(i+1) - y(i)
      
      val incObjX = min(x(i+1),y(j)) 
      val incObjY = min(x(i),y(j+1))

      (incObjX - incObjY, incX - incY) match {
        case (a,b) if (a < 0) => 

      }


      val dirMinX =  //Direction for minimising objective  
      val dirInc = incX - incY match { //Direction of least increment
        case > 0 => 
      }

      val branchPoint = x(i) == y(j) //

      //Pick direction
      val states = (incx - incy) match {
        case d if d > 0 => 
          val ns = newState(s.i,s.j+1) // Best direction
          if (eval(i+1,j) == ns.v)
            ns :: newState(s.i+1,s.j) :: Nil //Branch in terms of min. Will need this branch for later
          else
            ns :: Nil
        case d if d < 0 => 
          val ns = newState(s.i+1,s.j)
          if (eval(i,j+1) == ns.v)
            ns :: newState(s.i,s.j+1) :: Nil
          else
            ns :: Nil
        case default => 
          newState(s.i+1,s.j) :: newState(s.i,s.j+1) :: Nil
          //Branch in both directions. 
          //Note: i+1,j+1 possible iff x or y has duplicates
      }


    }

    val queue = 

    def search(s : State, lk : Int) : State {
      if(lk == k)
        return Some(s)
      queue ++= expand(s)
      queue.isEmpty match {
        case true => None
        case false =>
          val ns = queue.dequeue
          println(ns)
          search(ns,lk+1)
      }
    }

    val s = State(0,0,1)
    search(s).map(eval(_))
  }
 */

}
