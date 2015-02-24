package org.wahoo
package gameObject
package light

trait NotLit extends GameObject
{
	override def lit = false
}

trait PointGlow extends PointLight with NotLit
