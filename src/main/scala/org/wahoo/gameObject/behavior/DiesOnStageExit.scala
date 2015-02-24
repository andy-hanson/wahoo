package org.wahoo
package gameObject
package behavior

trait DiesOnStageExit extends InArena
{
	override def onStageExit()
	{
		die()
	}
}
