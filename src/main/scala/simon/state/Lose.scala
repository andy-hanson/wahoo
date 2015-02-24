package simon
package state

import gameObject.ScoreCounter

/** State that ends the game.
  * Pressing Start restarts the game.
  */
class Lose(score:Int) extends StartWaitState
{
	override def initObjects =
		super.initObjects :+ new ScoreCounter(score)

	def onStart()
	{
		main replaceState new Title
	}
}
