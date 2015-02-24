package org.wahoo

import math.implicits

import scala.language.implicitConversions

/** Common imports, joined together. */
package object common {
	def Scalar =
		math.Scalar
	type Scalar =
		math.Scalar

	implicit def float2Scalar(x:Float) =
		implicits.float2Scalar(x)
	implicit def double2Scalar(x:Double) =
		implicits.double2Scalar(x)
	implicit def int2Zazzy(a:Int) =
		implicits.int2Zazzy(a)

	def Color =
		color.Color
	type Color =
		color.Color

	type Shape =
		shape.Shape

	def Vec =
		shape.Vec
	type Vec =
		shape.Vec
	def BackOrForth =
		shape.BackOrForth
	type BackOrForth =
		shape.BackOrForth
	def DownOrUp =
		shape.DownOrUp
	type DownOrUp =
		shape.DownOrUp
	def LeftOrRight =
		shape.LeftOrRight
	type LeftOrRight =
		shape.LeftOrRight
}
