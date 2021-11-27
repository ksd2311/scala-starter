package com.ksd.basics.utils

import scala.annotation.tailrec

/**
 * Created by Kartik Deshamukh on Apr 10, 2021
 */

object HelloWorld2 extends App {
  println("Hello World")
  val list2 = (1 to 20).toList
  println("Max option >> " + list2.maxOption)

  def sum(list: List[Int]): Int = {
    list match {
      case ::(head, next) => head + sum(next)
      case Nil => {
        val stackTraces = Thread.currentThread().getStackTrace
        println("Stack trace")
        stackTraces.filter(_.getMethodName == "sum").foreach(println)
        println("result : ")
        0
      }
    }
  }

  @tailrec
  def sumWithTailRecursion(list: List[Int], sum: Int): Int = {
    list match {
      case ::(head, next) => sumWithTailRecursion(next, head + sum)
      case Nil => {
        val stackTraces = Thread.currentThread().getStackTrace
        println("Stack trace")
        stackTraces.filter(_.getMethodName == "sum").foreach(println)
        println("result : ")
        sum
      }
    }
  }

  println(sumWithTailRecursion((1 to 10000).toList, 0))

  println(sum((1 to 10).toList))


  trait Animal

  class Dog extends Animal

  class Cat extends Animal

  class Elephant extends Animal

  class Beagle extends Dog


  def getAnimal[A <: Animal] = {
    // possible ,
  }

  def getBullDog[A >: Beagle] = {
    //possibilities
  }
Left
  getBullDog[_]

  List[Int]()

}

trait Employee
