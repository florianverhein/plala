package com.verhein.florian.plala.exercise2

import scala.collection._

case class Edge[N,W](v1:N,v2:N,w:W) {
  override def toString = v1 + "--" + w + "-->" + v2
}

// Mutable graph
class Graph[N,W](val edges : mutable.Set[Edge[N,W]], val vertices: mutable.Set[N]) {

  import scala.collection.mutable._

  def += (e : Edge[N,W]) = {
    edges += e
    vertices += e.v1
    vertices += e.v2
    this
  }

  def ++= (g : Graph[N,W]) = {
    edges ++= g.edges
    vertices ++= g.vertices 
    this
  }

  override def toString = 
    edges.toString
}

object Graph {

  def apply[N,W]() : Graph[N,W] = this(Set[Edge[N,W]]())

  def apply[N,W](edges : Set[Edge[N,W]]) : Graph[N,W] = {
    val g = new Graph[N,W](
      mutable.HashSet[Edge[N,W]](), 
      mutable.HashSet[N]())
    edges foreach (e => g += e)
    g
  }

  def sample() = {

    type E = Edge[String,Double]
    def E(v1:String,v2:String,w:Double) = Edge(v1,v2,w)

    Graph[String,Double](
      Set[E]() +
        E("A","B",7) +
        E("A","D",5) +
        E("B","C",8) +
        E("B","D",9) +
        E("B","E",7) +
        E("C","E",5) +
        E("D","E",15) +
        E("D","F",6) +
        E("E","F",8) +
        E("E","G",9) +
        E("F","G",11)
    )
  }

  def random(v : Int, e : Int) = {
    import scala.util.Random
    val rand = new Random
    val g = this.apply[Int,Double]
    Range(0,e) foreach {
      _ => g += Edge[Int,Double](rand.nextInt(v),rand.nextInt(v),rand.nextDouble)
    }
    g
  }
}

object SpanningTree {

  def findConnected[N,W](connected : Set[Graph[N,W]], e : Edge[N,W]) : Set[Graph[N,W]]= {
    connected.collect{
      case g if g.vertices.contains(e.v1) ||
          g.vertices.contains(e.v2) => g
    }
  }

  def stitch[N,W](trees : Set[Graph[N,W]], e : Edge[N,W]) : Graph[N,W] = {
    //Know they are connected when and only when adding e, so can just union them
    val g = Graph[N,W](Set(e))
    trees.foreach(gnew => g ++= gnew)
    g
  }

  def completed[N,W](connected : Set[Graph[N,W]], g:Graph[N,W]) = {
    connected.size == 1 && connected.iterator.next.vertices.size == g.vertices.size
    //found single spanning tree
  }

  def apply[N,W](g : Graph[N,W ])(implicit f : W => Ordered[W]) = {

    val edgeOrdering = Ordering.by[Edge[N,W],W](e => e.w)
    val connected = mutable.Set[Graph[N,W]]()

    //var unconnected = mutable.PriorityQueue[Edge[N,W]]()(edgeOrdering)
    //g.edges foreach (e => unconnected.enqueue(e))

    val unconnected = (mutable.Queue() ++ g.edges.toList.sorted(edgeOrdering))
      .filter (e => e.v1 != e.v2) // Cycles useless

    while (!unconnected.isEmpty && !completed(connected,g)) {
      val e = unconnected.dequeue
      println(e)
      findConnected(connected,e) match {
        case trees if (trees.size == 0) => 
          connected += Graph(Set(e)) //matches no existing tree, create new one
        case trees if (trees.size == 1) => //intersects one existing tree, add..
            val tree = trees.iterator.next 
            if (!(tree.vertices.contains(e.v1) && tree.vertices.contains(e.v2))) 
              tree += e  //.. if does not introduce a cycle
        case trees =>                  //connects at least 2 existing trees, stitch them together
          val stitched = stitch(trees, e)
          connected --= trees
          connected += stitched
      }
      println(connected)
      println("---")
    }

    connected
  }

  def main(args : Array[String]) : Unit = {

    val g = args.length match {
      case 2 => 
        val nv = args(0).toInt
        val ne = args(1).toInt
        Graph.random(nv,ne)
      case default => Graph.sample
    }

    println(g)
    val st = SpanningTree(g)
    println(st)
    Unit
  }
}




