package simon

import
	state.Title

import
	org.wahoo.WahooMain

// Starts up the game.
object Simon
	extends App
{
	new Simon().play()
}

/** Handles state switching and game-wide issues. */
class Simon
	extends WahooMain
{
	def title =
		"Return of the Revenge of Simon"

	def WindowDimension =
		(800, 800)

	def StartGameState =
		new Title
}
