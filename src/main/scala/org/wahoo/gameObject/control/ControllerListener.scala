package org.wahoo
package gameObject
package control

/** Objects of this trait listen for ButtonPressed and ButtonReleased commands
  * from the state.stateObj.Controller.
  * All objects can also use the[Controller].isButtonDown for another method of control.
  */
@Catalogued
trait ControllerListener
	extends GameObject
{
	private def controller =
		the[Controller]

	def buttonPressed(b:Button) { }

	def buttonReleased(b:Button) { }

	def isButtonDown(b:Button) =
		controller isButtonDown b

	def isButtonUp(b:Button) =
		controller isButtonUp  b

	def joyStick =
		controller.joyStick

	def nthJoyStick(n:Int) =
		controller.nthJoyStick(n)

	//def joyStickX = controller.joyStickX
	//def joyStickY = controller.joyStickY
}
