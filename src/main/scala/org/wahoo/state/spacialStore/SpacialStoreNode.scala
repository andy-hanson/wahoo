package org.wahoo
package state
package spacialStore

import
	collection.{Bag, SortedBag},
	gameObject.{GameObject, InArena, StepsOffscreen},
	shape.Shape

import scala.reflect.classTag

//@todo: no stepObjects, just objects
abstract class SpacialStoreNode
	extends StoresByType
{
	def shape: Shape

	/** Bag of objects that only step if they are near the camera.
		* This is all objects that are not StepsOffscreens.
		*/
	var stepObjects = Bag.empty[InArena]

	/** Bag of every object in this ABNode. */
	var objects =
		SortedBag.empty[InArena](
			classTag[InArena],
			GameObject.DrawOrdering.asInstanceOf[Ordering[InArena]])

	/** Add an object to this Node.
		* It has already been checked that this Node is the right location for it.
		* Only puts it in the list of objects to step if it does not always step anyway.
		*/
	def add(o:InArena) {
		require(o.node != this)
		require(shape.contains(o.shape),
			s"$o at ${o.shape} should not be added to $this")
		require(!contains(o),
			s"$o added to $this twice!")

		o.node = this
		addToByType(o)
		objects += o
		o match {
			case s:StepsOffscreen =>
				// do nothing
			case _ =>
				stepObjects += o
		}
	}

	/** Remove an object from this Node.
		* Only takes it out of the list of objects to step if it was put there in add().
		*/
	def remove(o:InArena)
	{
		require(o.node == this)
		//I might contain its shape, if it is being removed due to death rather than to movement.
		assert(contains(o), s"tried to remove $o, but not contained in $objects")

		o.node = null
		removeFromByType(o)
		objects -= o
		o match {
			case o:StepsOffscreen =>
				// do nothing
			case _ =>
				stepObjects -= o
		}
	}

	def contains(o:InArena) =
		objects contains o
}
