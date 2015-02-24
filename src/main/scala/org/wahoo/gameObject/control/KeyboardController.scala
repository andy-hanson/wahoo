package org.wahoo
package gameObject
package control

import
	collection.BiMap,
	math.implicits._,
	shape.Vec

import org.lwjgl.input.Keyboard

/** Default controller using the keyboard.
  * Arrow keys are used to simulate a joystick.
  */
case class KeyboardController() extends Controller
{
	def nJoySticks =
		2

	private def joyButtons: Seq[(Int, Int, Int, Int)] =
		Seq(
			(Keyboard.KEY_LEFT, Keyboard.KEY_RIGHT,
				Keyboard.KEY_DOWN, Keyboard.KEY_UP),
			(Keyboard.KEY_A, Keyboard.KEY_D,
				Keyboard.KEY_S, Keyboard.KEY_W)
		)

	val keyButtonMap: BiMap[Button, Int] =
		BiMap(
			Button.A -> Keyboard.KEY_SPACE,
			Button.B -> Keyboard.KEY_LSHIFT,
			Button.X -> Keyboard.KEY_Z,
			Button.Y -> Keyboard.KEY_X,
			Button.L -> Keyboard.KEY_TAB,
			Button.R -> Keyboard.KEY_Q,
			Button.Start -> Keyboard.KEY_RETURN)

	/** Joystick will be simulated by the four arrow keys. */
	var joySticks: Array[Vec] =
		Array(Vec.Zero, Vec.Zero)

	override def step()
	{
		super.step()
		//Read in all new presses from the keyboard.
		//This does not affect the keyDown function.
		while (Keyboard.next()) {
			keyButtonMap.get(Keyboard.getEventKey()) match {
				case Some(b:Button) =>
					if (Keyboard.getEventKeyState())
						buttonPressed(b)
					else
						buttonReleased(b)
				case None =>
					// Some other key pressed. Ignore it.
			}
		}

		joySticks =
			(for (
				(l, r, d, u) <- joyButtons;
				x = (if (isKeyDown(l)) -1 else 0) + (if (isKeyDown(r)) 1 else 0);
				y = (if (isKeyDown(d)) -1 else 0) + (if (isKeyDown(u)) 1 else 0))
				yield Vec(x, y).unitOrZero
			).toArray
	}

	def nthJoyStick(n:Int) =
		joySticks(n)

	private def isKeyDown(key:Int) =
		Keyboard isKeyDown key

	/** Whether the given key is currently down.
		* Unlike keyJustPressed, will return true as long as the key stays down.
		*/
	def isButtonDown(b:Button) =
		isKeyDown(keyButtonMap(b))
}

