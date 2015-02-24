package org.wahoo
package gameObject

import shape.Vec

/** SimulatesVel will have a vel variable,
  * but will not use it to move.
  */
trait SimulatesVel extends HasShape {
	private var _vel = Vec.Zero
	override def vel = _vel
	def vel_=(newVel:Vec)
	{
		_vel = newVel
	}
}
