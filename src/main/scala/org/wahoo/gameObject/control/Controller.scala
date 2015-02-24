package org.wahoo
package gameObject
package control

import shape.Vec

/** Unique object for querying controls.
  * Different controls can be used by overriding level.controller()
  * Button names are analogous to those on a GameCube controller.
  * The controller is required to be unique, and is automatically added to every Level.
  */

trait Controller extends GameObject with Unique
{
	def nJoySticks: Int

	def joyStick =
		nthJoyStick(0)

	def nthJoyStick(n:Int): Vec

	def isButtonDown(b:Button): Boolean

	/** Whether the given key is currently up.
		* Unlike keyJustReleased, will return true as long as the key stays up.
		*/
	def isButtonUp(b:Button) =
		!isButtonDown(b)

	protected def buttonPressed (button:Button)
	{
		each[ControllerListener] { listener =>
			listener buttonPressed button
		}
	}

	protected def buttonReleased(button:Button)
	{
		each[ControllerListener] { listener =>
			listener buttonReleased button
		}
	}
}
