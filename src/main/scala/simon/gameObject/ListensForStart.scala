package simon
package gameObject

import
	org.wahoo.gameObject.control.{Button, ControllerListener}

/** Calls onStart() whenever Start (Enter) is pressed. */
trait ListensForStart
	extends ControllerListener
{
	def onStart()

	override def buttonPressed(button:Button)
	{
		button match {
			case Button.Start =>
				onStart()
			case _ =>
				// do nothing
		}
	}

}