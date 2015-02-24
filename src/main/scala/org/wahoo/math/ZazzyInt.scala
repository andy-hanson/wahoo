package org.wahoo
package math

import implicits._

object ZazzyInt {
	val lgTable =
		new Array[Int](256)

	lgTable(0) = 0
	lgTable(1) = 0

	for (i <- 2 until 256)
		lgTable(i) = 1 + lgTable(i / 2)
}

class ZazzyInt(val a:Int)
	extends AnyVal
{
	def isPowerOf2 =
		(a != 0) && isPowerOf2OrZero

	def isPowerOf2OrZero =
		(a & a - 1) == 0

	def positive =
		a > 0

	def negative =
		a < 0

	def nonPositive =
		a <= 0

	def nonNegative =
		a >= 0

	def sign =
		if (positive)
			1
		else if (negative)
			-1
		else 0

	def half =
		a >> 1

	def twice =
		a << 1

	def even =
		(a & 1) == 0

	def odd =
		!even

	def exp2 =
		1 << a

	def **(b:Int): Int =
	{
		if (b == 0)
			1
		else if (b.even)
			(this.square ** b.half)
		else
			(this.square ** b.half) * a
	}

	/** The lowest power of two that's at least this much. */
	def nextPowerOf2: Int =
	{
		require(a > 0)
		var x = a - 1
		//Convert x to a bunch of 1s.
		for (shift <- Array(1, 2, 4, 8, 16))
			x = (x >> shift) | x
		//Add 1. Now it's a power of 2!
		x + 1
	}

	def divisibleBy(by:Int) =
		a % by == 0

	def inRange(min:Int, max:Int) =
		a >= min && a <= max

	def inRangeExcludingMin(min:Int, max:Int) =
		a >  min && a <= max

	def inRangeExcludingMax(min:Int, max:Int) =
		a >= min && a <  max

	def inRangeExclusive   (min:Int, max:Int) =
		a >  min && a <  max

	def average(b:Int): Double =
		(a + b) / 2.0

	def averageInt(b:Int): Int =
		(a + b) / 2

	def *(s:Scalar): Scalar = s * a
	//def /(s:Scalar): Scalar = a / s.toFloat

	def limit(min:Int, max:Int): Int =
	{
		require(min <= max)
		if (a < min)
			min
		else if (a > max)
			max
		else
			a
	}

	def limitExcludingMin(min:Int, max:Int): Int =
	{
		require(min < max)
		if (a <= min)
			min + 1
		else if (a > max)
			max
		else
			a
	}

	def limitExcludingMax(min:Int, max:Int): Int =
	{
		require(min < max)
		if (a < min)
			min
		else if (a >= max)
			max - 1
		else
			a
	}

	def limitExclusive(min:Int, max:Int): Int =
	{
		require(min + 1 < max)
		if (a <= min)
			min + 1
		else if (a >= max)
			max - 1
		else
			a
	}

	def times[A](f: => A)
	{
		var i = a
		while (i != 0) {
			f
			i -= 1
		}
	}

	/** Log root 2. */
	def lg: Int =
	{
		if ((a & 0xff) == 0)
			ZazzyInt.lgTable(a)
		else
			8 + (a >> 8).lg
	}

	def square =
		a * a
}
