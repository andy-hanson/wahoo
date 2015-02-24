package org.wahoo
package gameObject
package loader

import resource.{Animation, AnimationData, MovingAnimationData, MovingAnimationLoop}

import scala.collection.mutable

final class SynchronizedAnimationMaster
	extends GameObject
	with Unique
{
	private val map =
		mutable.WeakHashMap.empty[AnimationData, SynchronizedAnimation]

	def get(data:AnimationData): SynchronizedAnimation =
	{
		require(data.synchronized)
		data match {
			case mad:MovingAnimationData if mad.loop =>
				map.getOrElseUpdate(data, new SynchronizedAnimation(mad))
			case _ =>
				throw new RuntimeException("Sorry, not supported!")
		}
	}

	override def step()
	{
		super.step()
		for (ani <- map.values)
			ani.advanceTime()
	}

	private def remove(s:SynchronizedAnimation)
	{
		map -= s.data
	}
}

private[loader] class SynchronizedAnimation(
	data:MovingAnimationData)
	extends MovingAnimationLoop(data)
{
	override def step() { }
	override def synchronized = true
}
