package org.wahoo
package gameObject
package time

import scala.collection.mutable

trait UsesTimers extends TracksTime
{
	private val timers =
		mutable.PriorityQueue[Timer]()

	override def step()
	{
		super.step()

		while (timers.nonEmpty && timers.head.time <= time)
			timers.dequeue().exec()
	}


	/** Call an action after time frames. */
	def after(offset:Int)(action: => Unit)
	{
		timers += Timer(time + offset, () => action)
	}

	def afterSynchronized(offset:Int)(action: => Unit)
	{
		def aft()
		{
			time % offset match {
				case 0 =>
					action
				case offBy =>
					after(offset - offBy)(aft)
			}
		}
		after(offset)(aft)
	}

	/** Repeat an action every period frames.
		* Timing may become off if this object does not update offscreen;
		*   for synchronized repeating see repeatSyncrhonized.
		*/
	def repeat(period:Int)(action: => Unit)
	{
		def recurse()
		{
			action
			after(period)(recurse)
		}
		after(period)(recurse)
	}

	/** Like repeat, but assures the action will only happen on exact multiples of period.
		* Useful if many different objects should be doing this at the same time.
		*/
	def repeatSynchronized(period:Int)(action: => Unit) {
		def recurse()
		{
			time % period match {
				case 0 =>
					action
					after(period)(recurse)
				case offBy =>
					after(period - offBy)(recurse)
			}
		}
		after(period)(recurse)
	}
}
