package org.wahoo
package gameObject
package time

/** Represents something that will be called at the specified time. */
case class Timer(time:Int, action:() => Unit) extends Ordered[Timer]
{
	// Lower time, higher priority.
	def compare(t:Timer) =
		t.time - time

	def exec()
	{
		action()
	}
}
