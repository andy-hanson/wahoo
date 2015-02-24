package org.wahoo
package color

import math.implicits._, math.Scalar

import scala.util.Random

object HSV {
	//val Amp  : Short = 1 << 10
	//val AmpShiftBits = 10
	//val random = new Random()
	def randomHue() = math.randomScalar() * 6//arandom.nextFloat() * 6
}

case class HSV(var hue:Scalar, var sat:Scalar, var vel:Scalar) {
	def toRGB = {
		//The color can be composed as a major part, a leaned-to part, and a minor part.

		//Which of the 6 parts of the color wheel is hue in?
		val sixthOfHueWheel = hue.toInt

		//Saturation times the fractional part of the hue, scaled to [0,1].
		val satTimesHueRemainder = sat * (hue % 1)

		val unSaturation = 1.0 - sat

		val minor = unSaturation * vel

		//Useful when color is leaning right: r->g, g->b, or b->r.
		val leanRight = (unSaturation + satTimesHueRemainder) * vel

		//Useful when color is leaning left: r->b, b->g, or g->r.
		val leanLeft = (1 - satTimesHueRemainder) * vel

		sixthOfHueWheel match {
			case 0 => Color(vel      , leanRight, minor    ) // Red   leaning to green.
			case 1 => Color(leanLeft , vel      , minor    ) // Green leaning to red.
			case 2 => Color(minor    , vel      , leanRight) // Green leaning to blue.
			case 3 => Color(minor    , leanLeft , vel      ) // Blue  leaning to green.
			case 4 => Color(leanRight, minor    , vel      ) // Blue  leaning to red.
			case 5 => Color(vel      , minor    , leanLeft ) // Red   leaning to blue.
		}
	}
}

/** S and V are in range [0..Color.Amp]. H is in range [0..Color.Amp*6) */
/*case class HSV private(var hue:Scalar, var sat:Short, var vel:Short) {
	def toRGB = {
		// The color can be composed into a major part,
		// a leaned-to part, and a minor part.

		// Which of 6 parts of the color wheel is hue in?
		//   Judge by the first 3 digits.
		val sixthOfHueWheel = hue / HSV.Amp

		// Saturation times the part of hue past the first 3 digits, scaled to ColorAmp.
		val satTimesHueRemainder = (sat * (hue % HSV.Amp)) >> HSV.AmpShiftBits

		val unSaturation = HSV.Amp - sat

		// Part of the color due to graying out.
		// Applies to the least major component.
		val minor: Short = (unSaturation * vel >> HSV.AmpShiftBits).toShort

		// Useful when the color is leaning right: r->g, g->b, or b->r.
		val leanRight: Short =
			( (unSaturation + satTimesHueRemainder) * vel >> HSV.AmpShiftBits ).toShort
		// Useful when the color is leaning left : r->b, b->g, or g->r.
		val leanLeft: Short =
			( (HSV.Amp - satTimesHueRemainder)      * vel >> HSV.AmpShiftBits ).toShort

		(sixthOfHueWheel: @switch) match {
			case 0 => Color(vel      , leanRight, minor    ) // Red leaning to green.
			case 1 => Color(leanLeft , vel      , minor    ) // Green leaning to red.
			case 2 => Color(minor    , vel      , leanRight) // Green leaning to blue.
			case 3 => Color(minor    , leanLeft , vel      ) // Blue leaning to green.
			case 4 => Color(leanRight, minor    , vel      ) // Blue leaning to red.
			case 5 => Color(vel      , minor    , leanLeft ) // Red leaning to blue.
		}
	}
}*/

