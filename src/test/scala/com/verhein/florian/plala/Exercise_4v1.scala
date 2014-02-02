package com.verhein.florian.plala.exercise4

import org.specs2._

class Exercise_4v1Spec extends Specification { def is = s2"""

    maxHeight should work for
      empty        $m1
      single       $m2
      example      $m3

    volumeHeld should work for
      empty                              $e1
      single bar                         $e2
      increase max decrease              $e3
      simple                             $e4
      double max                         $e5 
"""
           
  val lm = Exercise_4v1.lastMax _
  val vh = Exercise_4v1.volumeHeld(_ : Array[Double],1.0)

  def m1 = lm(Array()) == (-1,0.0)
  def m2 = lm(Array(1.0)) == (0,1.0)
  def m3 = lm(Array(3.0,2.0,3.0,2.0,3.0,2.0)) == (4,3.0)

  def e1 = vh(Array()) == 0
  def e2 = vh(Array(1.0)) == 0
  def e3 = vh(Array(1.0,2.0,3.0,2.0)) == 0
  def e4 = vh(Array(1.0,2.0,1.0,3.0,1.0,2.0)) == 2.0
  def e5 = vh(Array(3.0,2.0,3.0,2.0,3.0,2.0)) == 2.0

}
