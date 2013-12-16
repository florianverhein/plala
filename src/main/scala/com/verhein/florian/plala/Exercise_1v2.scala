package com.verhein.florian.plala

object Excercise_1v2 {

  import scala.math._
  import collection._

  case class State[A](seen : Set[A])

  def expand(s : String) = {
    ("0" :: "1" :: Nil) map (x => s.tail + x)
  }

  def goal[A](k : Int)(s : State[A]) = s.seen.size == pow(2,k)

  def dfs[A](start : A, s : State[A], expand : A => List[A], goal : State[A] => Boolean) : Option[List[A]] = {
    if (goal(s))
      return Some(start :: Nil)
    for (x <- expand(start)) {
      if (! s.seen(x) && x != start) {
        val r  = dfs(x, State[A](s.seen + x), expand, goal)
        if (!r.isEmpty) 
          return Some(start :: r.get)
      }
    }
    None
  }

  def main(args : Array[String]) = {
    val k = args(0).toInt

    def rep[A](a : A, k : Int) = Range(0,k) map ( _ => a) mkString

    val res = dfs(rep("0",k), State(Set[String]()), expand, goal[String](k))

    println(res) // complete cycle

    println(res match {
      case Some(p) => p.tail.map(x => x(0)).mkString //tail to prevent dup of start 
      case default => "No found"
    })
  }
}


