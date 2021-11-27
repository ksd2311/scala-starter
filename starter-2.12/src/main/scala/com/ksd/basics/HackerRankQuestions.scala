package com.ksd.basics

import scala.annotation.tailrec
import scala.math.BigDecimal.RoundingMode

/**
  * Created by ksdes on Apr 10, 2021
  */

object HackerRankQuestions extends App {
  def compareTriplets(a: Array[Int], b: Array[Int]): Array[Int] = {
    // Write your code here
    var aliceScore = 0;
    var bobScore   = 0;
    a.toList.zipWithIndex.foreach{
      case (i, index) =>
        if (i > b(index)) {
          aliceScore = aliceScore + 1
        } else if (i < b(index)) {
          bobScore = bobScore + 1
        } else {}
    }
    Array(aliceScore, bobScore)
  }

  //  println{
  //    compareTriplets(Array(5, 6, 7), Array(3, 6, 10)).toList
  //  }


  def aVeryBigSum(ar: Array[Long]): Long = {
    // Write your code here
    ar.sum
  }

  //  println(aVeryBigSum(Array(5,
  //    1000000001, 1000000002, 1000000003, 1000000004, 1000000005)))


  def diagonalDifference(arr: Array[Array[Int]]): Int = {
    val list = arr.filter(_.length > 1)
    // Write your code here

    val leftDiagonalList  = leftDiagonal(list)
    val rightDiagonalList = rightDiagonal(list)

    val res = leftDiagonalList.sum - rightDiagonalList.sum
    res.abs
  }


  val res = diagonalDifference(Array(
    Array(3),
    Array(11, 2, 4),
    Array(4, 5, 6),
    Array(10, 8, -12),
  ))

  //  println(res)

  def diag(m: Array[Array[Int]]) = {
    val (m1, m2) = m.zipWithIndex.
      map{ case (l, i) => val (l1, l2) = l.splitAt(l.size - i); l1 ++ l2 }.
      transpose.zipWithIndex.
      map{ case (l, i) => l.splitAt(i + 1) }.
      unzip
    m1 ++ m2.init
  }

  def rightDiagonal(arr: Array[Array[Int]], res: Array[Int] = Nil.toArray): Array[Int] = {
    if (arr.isEmpty) res
    else
      rightDiagonal(arr.tail.map(_.tail), res :+ arr.head.head)
  }

  def leftDiagonal(arr: Array[Array[Int]], res: Array[Int] = Nil.toArray): Array[Int] = {
    if (arr.isEmpty) res
    else
      leftDiagonal(arr.tail.map(_.init), res :+ arr.head.last)
  }


  def plusMinus(arr: Array[Int]) = {
    // Write your code here
    val totalSize        = arr.length.toDouble
    val positiveElements = arr.count(_ > 0)
    val negativeElements = arr.count(_ < 0)
    val zeroElements     = arr.count(_ == 0)

    def roundOff(value: Double) = BigDecimal(value).setScale(10, RoundingMode.HALF_UP)

    println(roundOff(positiveElements / totalSize))
    println(roundOff(negativeElements / totalSize))
    println(roundOff(zeroElements / totalSize))
  }

  //  plusMinus(Array(-4, 3, -9, 0, 4, 1))

  def staircase(n: Int) {
    // Write your code here
    val list = (1 to n).toList.map{ size =>
      (List.fill(n - size)(" ") ++ List.fill(size)("#"))
    }
    println(list.map(x => x.mkString("")).mkString("\n"))
  }

  //  staircase(6)


  def miniMaxSum(arr: Array[Int]) {
    val longs = arr.toList.map(_.toLong)

    // Write your code here
    @tailrec
    def internal2(list: List[(Long, Int)], acc: List[Long]): List[Long] = {
      list match {
        case Nil          => acc
        case head :: tail => {
          val others = longs.zipWithIndex.filterNot(_._2 == head._2)
          val sum    = others.map(_._1).sum
          internal2(tail, sum +: acc)

        }
      }
    }

    val finalResult = internal2(longs.zipWithIndex, Nil)
    if (finalResult.nonEmpty) {
      println(finalResult.min + " " + finalResult.max)
    } else println("0 0")
  }

  //  miniMaxSum(Array(256741038, 623958417, 467905213, 714532089, 938071625))
  //  miniMaxSum(Array(5, 5, 5, 5, 5))
  //  miniMaxSum(Array(5, 5, 5, 5, 6))

  def birthdayCakeCandles(candles: Array[Int]): Int = {
    // Write your code here
    if (candles.nonEmpty) {
      val maxHeight = candles.max
      candles.count(_ == maxHeight)
    } else 0
  }

  //  println(birthdayCakeCandles(Array(3, 2, 1, 3)))
  //  println(birthdayCakeCandles(Array(4, 4, 1, 3)))


  def timeConversion(s: String): String = {
    // Write your code here
    s.takeRight(2).toString match {
      case "AM" | "am" => s.take(2) match {
        case "12" => "00" + s.drop(2).dropRight(2)
        case any  => s.dropRight(2)
      }
      case "PM" | "pm" => s.take(2) match {
        case "12" => s.dropRight(2)
        case any  => {
          val to24Hrs = s.take(2).toInt + 12
          to24Hrs.formatted("%02d") + s.dropRight(2)
            .drop(2)
        }
      }
    }
  }
  //  println(timeConversion("07:05:45PM"))
  //  println(timeConversion("12:05:45PM"))
  //  println(timeConversion("07:05:45AM"))
  //  println(timeConversion("12:05:45AM"))

  def fill(num: Int, arr: List[Int]): List[Int] = {
    arr.flatMap{ x =>
      List.fill(num)(x)
    }
  }

  //  println(fill(3, List(1, 2, 3, 4)))
  def filter(delim: Int, arr: List[Int]): List[Int] = {
    for {
      value <- arr
      if value < delim
    } yield value
  }

  println(filter(3, List(10, 9, 8, 2, 7, 5, 1, 3, 0)))
}
