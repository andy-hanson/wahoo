package org.wahoo
package math

import scala.language.implicitConversions

package object implicits {
	implicit def float2Scalar(x:Float): Scalar =
		Scalar(x)

	implicit def double2Scalar(x:Double): Scalar =
		Scalar(x.toFloat)

	//implicit def int2Scalar(x:Int): Scalar =
	//	Scalar(x.toFloat)

	implicit def int2Zazzy(a:Int): ZazzyInt =
		new ZazzyInt(a)
}
