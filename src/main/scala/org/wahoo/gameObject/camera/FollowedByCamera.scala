package org.wahoo
package gameObject
package camera

trait FollowedByCamera extends InArena
{
	override def start()
	{
		super.start()
		the[Camera].follow(this)
	}
}
