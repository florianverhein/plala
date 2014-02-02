package com.verhein.florian.plala


object Sorting {

  def mergeSort[A : scala.reflect.ClassTag](x : Array[A])(implicit ord : Ordering[A]) = {

    //TODO swap x and y to avoid copying. 

    val y = new Array[A](x.length) //buffer

    def copyFromY(from : Int, till : Int) = {
      (from until till) map ( i => x(i) = y(i))
    }

    def splitAndMerge(from : Int = 0, till : Int = x.length) : Unit = {
      if (till - from > 1) {
        val mid = (from + till) / 2
        splitAndMerge(from,mid)
        splitAndMerge(mid,till)
        mergeIntoY(from,mid,till)
        copyFromY(from,till)
      }
    }

    def log(a : Array[A], from : Int, till : Int) = 
      println(a.mkString(",") + " -> " + a.view(from,till).mkString(","))

    def mergeIntoY(from : Int, mid : Int, till :Int) = {

      //log(x,from,till)
      var i = from
      var j = mid
      var yi = from

      while (i < mid || j < till) {
        def usei = {y(yi) = x(i); i += 1; yi += 1}
        def usej = {y(yi) = x(j); j += 1; yi += 1}
        if (i == mid)
          usej
        else if (j == till)
          usei
        else {
          if (ord.lteq(x(i),x(j)))
            usei
          else usej
        }
      }
      //log(y,from,till)
    }

    splitAndMerge()
  }

  def quickSort[A](x : Array[A])(implicit ord : Ordering[A]) = {

    def swap(i : Int, j : Int) = {
      val t = x(i)
      x(i) = x(j)
      x(j) = t
    }

    def selectPivot(from : Int, to : Int) = from + util.Random.nextInt(to-from)
    
    def partition(from : Int, to : Int, pivot : Int) : Int = {
      val pivotValue = x(pivot)
      swap(to,pivot)
      var si = from
      (from until to).map {
        case i => 
          if (ord.lteq(x(i),pivotValue)) {
            swap(i,si)
            si += 1
          }
      }
      swap(si,to)
      si
    }

    def sort(from : Int, to : Int) : Unit = {
      if (from < to) {
        val pivot = selectPivot(from,to)
        val si = partition(from,to,pivot)
        // Optimise tail recursion so done on longes part
        if (pivot < (to-from) / 2) {
          sort(from,si-1)
          sort(si+1,to)
        } else {
          sort(si+1,to)
          sort(from,si-1)
        }
      }
    }

    sort(0,x.length-1)

  }

  def main(args : Array[String]) : Unit = {

    val alg = args(0)
    val x = args(1).split(",").map(x => x.toInt)

    alg match {
      case "quick" => quickSort(x)
      case "merge" => mergeSort(x)
      case default => println("Unknown algorithm")
    }

    println(x.mkString(","))    
    Unit
  }

}


