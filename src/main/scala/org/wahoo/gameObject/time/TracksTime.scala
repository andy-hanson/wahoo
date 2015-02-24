package org.wahoo
package gameObject
package time

trait TracksTime extends GameObject
{
	var time = 0

	override def step()
	{
		super.step()
		time += 1
	}
}
