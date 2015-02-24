package org.wahoo
package gameObject
package logic

import state.GameState

import scala.reflect.ClassTag

trait AccessesClassMap
{
	def gameState: GameState

	def all[A <: GameObject : ClassTag] =
		gameState.all[A]

	/** Iterate over all objects of a type. */
	def each[A <: GameObject : ClassTag](f:A => Unit)
	{
		gameState.each[A](f)
	}

	/** Iterate over all objects of a type that satisfy some predicate. */
	def eachWhen[A <: GameObject : ClassTag](
		filter:A => Boolean)(
		f:A => Unit)
	{
		gameState.eachWhen[A](filter)(f)
	}
}
