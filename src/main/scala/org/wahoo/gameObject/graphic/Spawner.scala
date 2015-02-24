package org.wahoo
package gameObject
package graphic

import
	math.implicits._,
	math.Scalar,
	collection.Bag

/** Type for graphics objects that output a huge number of Spawn.
  * Spawn are not added to the GameState (or the ABTree).
  *
  */
trait Spawner[SpawneeType <: Spawn]
	extends GameObject
{
	var spawnees = Bag.empty[Spawn]

	def spawnee(): SpawneeType

	def spawnsPerFrame: Scalar

	var spawnsToDo: Scalar = 0

	// @todo: applyFilter { _.step(); _.alive)
	override def step()
	{
		super.step()

		spawnsToDo += spawnsPerFrame
		while (spawnsToDo.positive) {
			spawnees += spawnee()
			spawnsToDo -= 1
		}

		for (s <- spawnees)
			s.step()

		spawnees.foreach { _.step() }
	}

	override def draw()
	{
		super.draw()
		for (s <- spawnees)
			s.draw()
	}
}

trait Spawn extends HasShape
