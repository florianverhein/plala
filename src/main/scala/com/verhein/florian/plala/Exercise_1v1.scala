package com.verhein.florian.plala

import scalaz._
import collection._

case class Edge[N] (from: N, to: N)

class Graph[N] (
  val out : mutable.Map[N,mutable.Set[Edge[N]]],
  val in : mutable.Map[N,mutable.Set[Edge[N]]]
) {

  def add(n : N) : Graph[N] = {
    if (!out.contains(n)) {
        out(n) = mutable.Set[Edge[N]]()
        in(n) = mutable.Set[Edge[N]]()
    }
    this
  }

  def add(e : Edge[N]) : Graph[N] = {
    add(e.from) add e.to
    out(e.from) += e
    in(e.to) += e
    this
  }

  def outgoing(n : N) = out(n)

  def incoming(n : N) = in(n) 

  def size = out.size

  override def toString = out map {
    case (n,es) => n + " -> " + es.map( (x) => x.to ).mkString(",")
  } mkString("\n")
}

object Graph {

  def apply[N] = {
    val m = mutable.Map[N,mutable.Set[Edge[N]]]()
    new Graph(m,m.empty)
  }

  def findHamiltonianPath[N](g : Graph[N], start : N, seen : Set[N] = Set[N]()) : Option[List[N]] = {
    seen.size == g.size match {
      case true => Some(start :: Nil)
      case false =>
        for (e <- g.outgoing(start)) {
          if (!seen(e.to)) {
            val r = findHamiltonianPath(g,e.to,seen + e.to)
            if (!r.isEmpty)
              return Some(start :: r.get)
          }
        }
        return None
    }
  }
}

object Excercise_1v1 {

  def generate( bits : Int, tail : String = "") : Seq[String] = {
    (bits == 0) match {
      case true => Seq(tail)
      case false => generate(bits-1,"1" + tail) ++ 
        generate(bits-1,"0" + tail)
    }
  }

  def overlap(a: String, b : String) = b.startsWith(a.tail)

  def main(args : Array[String]) : Unit = {

    val bits = args(0).toInt
    val s = generate(bits)
    //println(s)

    val g = Graph[String]

    val log  = (for (
      a <- s; 
      b <- s
    ) yield (a,b)).flatMap {
      case (a,b) if (a != b && overlap(a,b)) => 
        g.add(Edge(a,b))
        Some(a + "->" + b)
      case default => None
    }

    println(log)

    println(g.size)
    println(g)

    val p = Graph.findHamiltonianPath(g, s(0))
    println(p)
    
    println(p match {
      case Some(p) => p.map(x => x(0)).mkString
      case None => "Not found"
    })

    Unit

  }

}

