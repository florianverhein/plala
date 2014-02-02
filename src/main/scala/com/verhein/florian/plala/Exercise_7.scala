package com.verhein.florian.plala

/** Given a sequence of numbers x_i,...,x_n compute 
  * the sequence of products p_i such that p_i is the product of all numbers except x_i 
  */
object Exercise_7 {

  //Division allowed
/*  def products(x : Array[Double]) = {
    val all = x.foldLeft(1){case (v,curr) => curr*x }
    x.map(v => all/v)
  }
 */
  //Only multiplication allowed
 /* def products2(x : Array[Double]) = {

    val fromRight = Array[Double](x.length)
    for (i <- (x.length -1) to 0 by -1) fromRight(i) = fromRight(i+1)*x(i)

      case (curr,v) = curr()
    }

  }
  */


}


