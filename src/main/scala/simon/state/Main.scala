package simon
package state

import
	gameObject.SimonMaster

import
	org.wahoo.common._,
	org.wahoo.gameObject.graphic.NamedBackground,
	org.wahoo.gameObject.control.KeyboardController,
	org.wahoo.shape.Rect,
	org.wahoo.state.GameState

/** Sets up the SimonMaster. */
class Main extends GameState
{
	def initObjects =
		Seq(
			new NamedBackground("simon/BG"),
			new KeyboardController,
			new SimonMaster
		)

	def ArenaSize =
		Vec(800, 800)
}
