package org.wahoo
package color

import math.implicits._, math.Scalar

import org.lwjgl.opengl.GL11

object Color {
	/** How brightly the colors show up on screen.
		* At 1.0 values map directly to gl floats.
		* At 2.0 a value must be twice as bright to use full display brightness.
		* Increase to make it brighter at the cost of highs standing out less.
		*/
	val Brightness = 1.0

	val Black = Color(0  , 0  , 0  )
	val DarkGray = Color(.25, .25, .25)
	val Gray = Color(.5 , .5 , .5 )
	val LightGray    = Color(.75, .75, .75)
	val White        = Color(1  , 1  , 1  )

	val Red          = Color(1  , 0  , 0  )
	val RedOrange    = Color(1  , .25, 0  )
	val Orange       = Color(1  , .5 , 0  )
	val YellowOrange = Color(1  , .75, 0  )

	val Yellow       = Color(1  , 1  , 0  )
	val YellowLeaf   = Color(.75, 1  , 0  )
	val Grass        = Color(.5 , 1  , 0  )
	val GreenGrass   = Color(.25, 1  , 0  )

	val Green        = Color(0  , 1  , 0  )
	val GreenNeon    = Color(0  , 1  , .25)
	val Neon         = Color(0  , 1  , .5 )
	val CyanNeon     = Color(0  , 1  , .75)

	val Cyan         = Color(0  , 1  , 1  )
	val CyanSea      = Color(0  , .75, 1  )
	val Sea          = Color(0  , .5 , 1  )
	val BlueSea      = Color(0  , .25, 1  )

	val Blue         = Color(0  , 0  , 1  )
	val BlueLavender = Color(.25, 0  , 1  )
	val Lavender     = Color(.5 , 0  , 1  )
	val PinkLavender = Color(.75, 0  , 1  )

	val Pink         = Color(1  , 0  , 1  )
	val PinkPeach    = Color(1  , 0  , .75)
	val Peach        = Color(1  , 0  , .5 )
	val RedPeach     = Color(1  , 0  , .25)

	def randomBrightSaturated(): Color = HSV(HSV.randomHue(), 1, 1).toRGB
}

case class Color(var red:Scalar, var green:Scalar, var blue:Scalar)
								 extends PartiallyOrdered[Color] {

	def tryCompareTo[B >: Color](b:B)(implicit evidence: B => PartiallyOrdered[B]) = b match {
		case c:Color =>
			if      (red < c.red && green < c.green && blue < c.blue) Some(-1)
			else if (red > c.red && green > c.green && blue > c.blue) Some( 1)
			else if (this == c) Some(0)
			else None
		case _ => None
	}

	def maxComponent =
		if (red > green)
			if (red > blue) red
			else blue
		else //green >= red
			if (green > blue) green
			else blue

	def isAbout(c:Color) = red.isAbout(c.red) && green.isAbout(c.green) && blue.isAbout(c.blue)

	def setTo(c:Color) { red = c.red; green = c.green; blue = c.blue }

	def +=(c:Color) { red += c.red; green += c.green; blue += c.blue }
	def -=(c:Color) { red -= c.red; green -= c.green; blue -= c.blue }
	def *=(c:Color) { red *= c.red; green *= c.green; blue *= c.blue }
	def /=(c:Color) { red /= c.red; green /= c.green; blue /= c.blue }

	def *=(s:Scalar) { red *= s; green *= s; blue *= s }
	def /=(s:Scalar) { this *= 1 / s }

	def +(c:Color) = Color(red + c.red, green + c.green, blue + c.blue)
	def -(c:Color) = Color(red - c.red, green - c.green, blue - c.blue)
	def *(c:Color) = Color(red * c.red, green * c.green, blue * c.blue)
	def /(c:Color) = Color(red / c.red, green / c.green, blue / c.blue)

	//def +(s:Scalar) = Color(red + s, green + s, blue + s)
	//def -(s:Scalar) = Color(red - s, green - s, blue - s)
	def *(s:Scalar) = Color(red * s, green * s, blue * s)
	def /(s:Scalar) = this * (1 / s)

	/** Converts to openGL representation. For debugging only! */
	//def glTriple = ( partToGL(red), partToGL(green), partToGL(blue) )

	def toGL(s:Scalar) = (s * Color.Brightness).toFloat

	/** Tell OpenGl to start using this color. */
	def applyGL() { GL11.glColor3f(toGL(red), toGL(green), toGL(blue)) }
	def applyGLClearColor() { GL11.glClearColor(toGL(red), toGL(green), toGL(blue), 1.0f) }


	//def brighterBy(amount:Scalar) = Color(red + amount, green + amount, blue + amount)
	//def darkerBy(amount:Scalar) = brighterBy(-amount)

	def darkBy  (factor:Scalar) = this * factor
	def brightBy(factor:Scalar) = this * factor

	def dark   =   darkBy(0.5)
	def bright = brightBy(0.5)
	/*
	def toHSV = {
		var vel = 0
		var min = 0
		if (red > green)
			if (red > blue) {
				vel = red
				if (green > blue)
					min = blue
				else
					min = green
			}
			else {
				vel = blue
				min = green
			}
		else {
			if (green > blue) {
				vel = green
				if (red > blue)
					min = blue
				else
					min = red
			}
			else {
				vel = blue
				min = red
			}
		}

		if (vel == min) return HSV(0, 0, vel) //Grey
		else {
			val delta = vel - min
			val sat = Amp * delta / vel

			hue = if (red == vel)
							if (green >= blue)
								( (green - blue) * Amp ) / delta
							else
								hue = 6 * Amp + ( (green - blue) * Amp ) / delta
						else if (green == vel)
							2 * Amp + ( (blue - red) * Amp ) / delta
						else
							4 * Amp + ( (red - green) * Amp) / delta
			HSV(hue, sat, vel)
		}
	}
	*/
}
