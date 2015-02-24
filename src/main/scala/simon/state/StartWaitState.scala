package simon
package state

import
	org.wahoo.gameObject.control.{KeyboardController, StartListener}

/** An ImageState that calls onStart when Start is pressed. */
abstract class StartWaitState
	extends ImageState
{
	def onStart()

	override def initObjects =
		super.initObjects ++ Seq(
			new KeyboardController,
			new StartListener(onStart)
		)
}
