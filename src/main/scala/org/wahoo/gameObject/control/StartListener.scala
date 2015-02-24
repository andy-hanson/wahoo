package org.wahoo
package gameObject
package control

class StartListener(action: () => Unit)
	extends GameObject
	with ControllerListener
{
	override def buttonPressed(button:Button)
	{
		super.buttonPressed(button)

		button match {
			case Button.Start =>
				action()
			case _ =>
				// do nothing
		}
	}
}
