package simon
package gameObject

import
	org.wahoo.common._,
	org.wahoo.gameObject.control.ControllerListener,
	org.wahoo.gameObject.logic.HasFlags,
	org.wahoo.shape.Side

/** Subsumes SimonSayer and SimonListener.
  * Waits for the player to match the input data.
  */
abstract class SimonInput(data:Seq[Side])
	extends ControllerListener
	with HasFlags
{
	def MaxSoundIndex =
		15

	/** Called when the player enters correctly. */
	def onInputSuccess() { }

	/** Called when the player enters incorrectly. */
	def onInputFail() { }

	/** Called when every in data has been correctly matched. */
	def onDone() { }

	private var dataIndex = 0

	/** The part of data that we are currently on. */
	def currentSide =
		data(dataIndex)

	// Indicates that player has reset their joyStick to 0 and can enter new input.
	// Accessed with isOn and turnOff.
	case object Ready extends Flag

	// Sounds will be in resources/sound/simon/gameObject/SimonInput
	override def SoundDirectoryName =
		"SimonInput"

	override def step()
	{
		super.step()

		if (joyStick.nonZero) {
			if (isOn(Ready)) {
				val side =
					// What side of 0?
					Vec.Zero collideSide joyStick

				if (side == currentSide) {
					val soundIndex =
						// We don't have sounds for > MaxSoundIndex
						dataIndex min MaxSoundIndex
					playSound(s"$soundIndex")

					dataIndex += 1

					if (dataIndex >= data.size) {
						onDone()
						the[SimonMaster].done()
					}
					else {
						onInputSuccess()
					}
				}
				else {
					playSound("wrong")
					onInputFail()
				}

				turnOff(Ready)
			}
		}
		else
			turnOn(Ready)
	}
}
