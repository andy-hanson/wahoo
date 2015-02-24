package org.wahoo
package gameObject

import logic.{AccessesCollidingClassMap, AccessesCollidingUniques},
			 shape.{Shape, Side},
			 state.spacialStore.SpacialStoreNode

import scala.reflect.ClassTag

/** An object that is in the SpacialStore.
  * Make sure to call updateInSpacialStore when it moves.
  * By default, this object will only update near the camera,
  * and will only draw on camera.
  * See UpdatesAlways for exceptions to the update rule.
  * If you want an object to draw offscreen,
  * you probably don't want it drawn in the camera's coordinates -
  * see DrawsBeforeCamera for this.
  */
trait InArena
	extends HasShape
	with AccessesCollidingClassMap
	with AccessesCollidingUniques
{
	var node: SpacialStoreNode = null

	override def lit =
		true

	/** Whether this object collides with another object.
		* Avoid using this is you can use eachColliding instead.
		*/
	def collides(o:InArena): Boolean =
	{
		require(o ne this, s"$this checking for collision with self")
		collides(o.shape)
	}

	/** Whether this object collides with the given shape.
		* This method should only be called by the SpacialStore.
		*/
	def collides(s:Shape) =
		shape.collides(s)

	/** The side (see shape.Side) of this another HasShape is on.
		* Should guarantee collision with it before calling this!
		*/
	def collideSide(o:InArena): Side =
		collideSide(o.shape)

	/** The side (see shape.Side) of this a shape is on.
		* Should guarantee collision with it before calling this!
		*/
	def collideSide(s:Shape) =
		shape.collideSide(s)

	def leftOf (o:InArena): Boolean =
		leftOf(o.shape)
	def rightOf(o:InArena): Boolean =
		rightOf(o.shape)
	def above(o:InArena): Boolean =
		above(o.shape)
	def below(o:InArena): Boolean =
		below(o.shape)
	def leftOf (s:Shape) =
		shape leftOf s
	def rightOf(s:Shape) =
		shape rightOf s
	def above(s:Shape) =
		shape above s
	def below(s:Shape) =
		shape below s

	/** Called by SpacialStore when this object leaves the tree. */
	def onStageExit()
	{
		throw new RuntimeException(
			s"$this left the valid arena of ${gameState.ArenaShape}.\n" +
			"Handle this by overriding onStageExit().")
	}

	/** Remove myself from a node upon leaving the game. */
	override def finish()
	{
		super.finish()
		//If we died due to stageExit, no need to ask node to remove me; it already has.
		if (node != null)
		{
			node.remove(this)
			node = null
		}
	}

	/** Default toString prints class, location, and state. Ex. "Brier@(0,0):Spike" */
	override def toString =
		s"${getClass.getSimpleName}@${shape.center}:$state"
}
