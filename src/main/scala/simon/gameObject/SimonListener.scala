package simon
package gameObject

import
	state.Lose

import
	org.wahoo.common._,
	org.wahoo.color.Color,
	org.wahoo.gameObject.GameObject,
	org.wahoo.gameObject.camera.Camera,
	org.wahoo.shape.{Circle, Rect, Side},
	org.wahoo.render

/** Waits for input and does not show the player what to input.
  * Does show a time limit and ends the game if it runs out.
  */
class SimonListener(data:Seq[Side])
	extends SimonInput(data)
{
	/** Number of frames from start of input until game over. */
	def MaxTimeLeft =
		50

	var timeLeft = MaxTimeLeft

	override def step()
	{
		super.step()

		timeLeft -= 1
		if (timeLeft <= 0) {
			playSound("outOfTime")
			onInputFail()
		}
	}

	override def draw()
	{
		super.draw()

		val pixelsPerFrameLeft =
			the[Camera].width / MaxTimeLeft

		val r =
			Rect centered (the[Camera].center, timeLeft * pixelsPerFrameLeft)

		render rect (r, lineWidth = 10, color = Color.Red)
	}

	override def onInputSuccess()
	{
		timeLeft = MaxTimeLeft
	}

	override def onInputFail()
	{
		// Moves us to the Lose state.
		// Lose will display what the score was.
		gameState.main replaceState new Lose(the[SimonMaster].score)
	}
}
