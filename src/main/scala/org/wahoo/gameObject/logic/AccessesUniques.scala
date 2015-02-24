package org.wahoo
package gameObject
package logic

import state.GameState

import scala.reflect.ClassTag

trait AccessesUniques
{
	def gameState: GameState

	/** Get the Unique object of a type.
		* Fails if it does not exist.
		* Use: {{val theImportantThing = the[ImportantThing]}}
		*/
	def the[A <: Unique : ClassTag]: A =
		gameState.the[A]

	/** Get the Unique object of a type or None if it doesn't exist. */
	def maybeThe[A <: Unique : ClassTag]: Option[A] =
		gameState.maybeThe[A]
}
