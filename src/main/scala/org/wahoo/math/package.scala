package org.wahoo

import math.implicits._

import scala.util.Random

/** Provides simple functions on numbers that scala's math package is missing. */
package object math {
	//def min[A](a:A, b:A)(implicit ordering:Ordering[A]): A = if (ordering.lt(a, b)) a else b
	//def max[A](a:A, b:A)(implicit ordering:Ordering[A]): A = if (ordering.lt(a, b)) b else a

	def toDigits(value:Int, nDigits:Int, base:Int = 10): Array[Int] =
	{
		val digits = new Array[Int](nDigits)
		(0 until nDigits).foldLeft(value) { (v, i) =>
			digits(i) = v % base
			v / base
		}
		digits.reverse
	}

	//@todo speed up by using floats, not doubles
	def atan2(y:Scalar, x:Scalar): Scalar =
		scala.math atan2 (y.toDouble, x.toDouble)

	val Pi : Scalar =
		scala.math.Pi
	val Pi2 =
		Pi.twice
	val Root2: Scalar =
		scala.math sqrt 2
	val InvRoot2 =
		Root2.half

	private val randomGenerator =
		new Random()

	def randomScalar() =
		Scalar(randomGenerator.nextFloat())
	def randomAngle() =
		randomScalar() * Pi2

	def random(max:Scalar): Scalar =
		randomScalar() * max
	def random(min:Scalar, max:Scalar): Scalar =
			min + randomScalar() * (max - min)
	def random(minMax:(Scalar, Scalar)): Scalar =
		random(minMax._1, minMax._2)

	def randomUpTo(maxExclusive:Int) =
		randomGenerator.nextInt(maxExclusive)
	def random(max:Int): Int =
		randomGenerator.nextInt(max + 1)
	def random(min:Int, max:Int): Int =
		min + random(max - min)
	def random(minMax:(Int, Int)): Int =
		random(minMax._1, minMax._2)

	def randomOf[A](a: => A, b: => A): A =
		if (randomUpTo(256) < 128)
			a
		else
			b

	def randomOf[A](t:Traversable[A]): A =
		(t drop randomUpTo(t.size)).head

}
