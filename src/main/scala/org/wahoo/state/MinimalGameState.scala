package org.wahoo
package state

/** The interface required by WahooMain. */
abstract class MinimalGameState
{
	val main =
		WahooMain.main
	def windowTitle: String
	def start()
	def step()
	def draw()
	def finish()
}
