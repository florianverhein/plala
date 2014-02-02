package com.verhein.florian.plala


object Exercise_10 {

  def fibonacci(n : Int) = {
    if (n <= 0) 0
    else (1 until n).foldLeft[(Int,Int)]((1,0)){
      case ((prev1,prev2),_) => (prev1 + prev2, prev1)
    }._1
  }

  def palindrome(str : String) = {
    def check(i : Int = 0) : Boolean = { 
      if (i >= str.length/2) true
      else if (str(i) == str(str.length - i - 1)) check(i+1)
      else false
    }
    check()
  }

  /** Suppose people are seated around a table and will present in turn (clockwise) starting 
    * from a chosesn starting person.
    * However, each person has a preferred presentation order (i.e. the person may want to be 3rd
    * but doesn't care who is 1st or 2nd, etc). Find the optimal starting person. 
    */
  def bestOrder(preferences : Array[Int]) = {
    
    val votes = Array.fill(preferences.length)(0)
    
    def increment(i : Int) = votes(i) = votes(i)+1
    
    preferences.foldLeft(0){
      case (curr,p) => 
        increment((preferences.length + curr - p) % preferences.length)
        curr + 1
    }
    
    votes.foldLeft((0,0,0)){
      case ((maxi,maxv,i),v) => 
        if (maxv < v) (i,v,i+1)
        else (maxi,maxv,i+1)
    }._1
  }


  def main(args : Array[String]) : Unit = {
    println(fibonacci(args(0).toInt))
    Unit
  }


}


