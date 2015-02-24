package org.wahoo
package math

import implicits._

object Scalar {
	//Accurate to 10 binary digits expected
	val RelativeErrorAllowed: Scalar =
		1.0 / 1024

	val MinValue: Scalar =
		Float.MinValue
	val MaxValue: Scalar =
		Float.MaxValue
}

case class Scalar(a:Float) extends AnyVal with Ordered[Scalar] {
	def + (b:Scalar): Scalar =
		a + b.a
	def - (b:Scalar): Scalar =
		a - b.a
	def * (b:Scalar): Scalar =
		a * b.a
	def / (b:Scalar): Scalar =
		a / b.a
	def % (b:Scalar): Scalar =
		a % b.a

	def compare(b:Scalar) =
		if (this < b) -1
		else if (this > b) 1
		else 0

	override def < (b:Scalar) =
		a <  b.a
	override def <=(b:Scalar) =
		a <= b.a
	override def > (b:Scalar) =
		a >  b.a
	override def >=(b:Scalar) =
		a >= b.a

	def sign: Int =
		if (negative)
			-1
		else if (positive)
			1
		else
			0

	/** Sign without possibility of 0. Returns 1 if I am 0. */
	def signOr1: Int =
		if (negative) -1 else 1

	def round: Int =
		scala.math round a

	def aboutLessThan(b:Scalar) =
		this < b || isAbout(b)
	def aboutMoreThan(b:Scalar) =
		this > b || isAbout(b)

	def half: Scalar =
		a / 2
	def twice: Scalar =
		a * 2

	def positive =
		a > 0
	def negative =
		a < 0
	def nonPositive =
		a <= 0
	def nonNegative =
		a >= 0

	/** Returns how many powers of 2 this is more than b,
		* or 0 if this is less than b.
		*/
	def powersOf2MoreThan(b:Scalar): Int =
		if (this <= b)
			0
		else
			(this / b).log2.ceil

	def timesPowerOf2(power:Int): Scalar =
	{
		require(power >= 0, s"timesPowerOf2 passed negative power $power")
		a * (1 << power)
	}

	def isAbout(b:Scalar) =
	{
		val diff = this - b
		diff.isAboutZero || (diff / b).isAboutZero
	}
	def isAboutZero =
		abs <= Scalar.RelativeErrorAllowed
	def notAbout(b:Scalar) =
		!isAbout(b)
	def notAboutZero =
		!isAboutZero

	def average(b:Scalar): Scalar =
		(a + b).half

	def unary_- : Scalar =
		-a

	def inverse: Scalar =
		1 / a

	def invsqrt: Scalar =
		1 / sqrt

	def toInt: Int =
		a.toInt

	def toFloat: Float =
		a

	def toDouble: Double =
		a.toDouble

	def abs: Scalar =
		scala.math abs a

	def isZero =
		a == 0

	def nonZero =
		a != 0

	def ==(i:Int) =
		a == i

	def !=(i:Int) =
		a != i

	def safeACos: Scalar =
		if (a > 1)
			0
		else if (a < -1)
			scala.math.Pi
		else
			a.acos

	def acos: Scalar =
		scala.math acos a

	def sqrt: Scalar =
		scala.math sqrt a

	def cos: Scalar =
		scala.math cos a

	def sin: Scalar =
		scala.math sin a

	def ln : Scalar =
		scala.math log a

	//@todo: really needed?
	def cosAndSin: (Scalar, Scalar) =
	{
		val kos = cos
		( kos, (1 - kos * kos).sqrt )
	}

	def ceil : Int =
		(scala.math ceil a).toInt
	def floor: Int =
		(scala.math floor a).toInt

	def ^(b:Scalar): Scalar =
		scala.math pow (a, b.a)
	def ^(b:Int): Scalar =
		scala.math pow (a, b)

	def square: Scalar =
		a * a

	def log2: Scalar =
		ln / 2.ln

	def isPowerOf2: Boolean =
		log2.isInteger

	def isInteger: Boolean =
		this == floor

	def nextPowerOf2: Scalar =
		scala.math pow (2, log2.ceil)

	def min(b:Scalar) =
		if (this < b)
			this
		else
			b

	def max(b:Scalar) =
		if (this > b)
			this
		else
			b

	def limit(low:Scalar, high:Scalar): Scalar =
		if (this < low)
			low
		else if (this > high)
			high
		else
			this

	def limit(lowHigh:(Scalar, Scalar)): Scalar =
		limit(lowHigh._1, lowHigh._2)

	def limitSize(magnitude:Scalar): Scalar =
		limit(-magnitude, magnitude)

	def reduce(by:Scalar): Scalar =
		if (this < 0)
			(this + by).toNonPositive
		else
			(this - by).toNonNegative

	def approach(value:Scalar, by:Scalar): Scalar =
		if (this < value)
			(this + by) min value
		else
			(this - by) max value

	/** The lowest positive number at least this much. */
	def toNonNegative: Scalar =
		if (this < 0)
			0
		else
			this

	/** The highest negative number at least this low. */
	def toNonPositive: Scalar =
		if (this > 0)
			0
		else
			this

	override def toString =
		a.toString
}
