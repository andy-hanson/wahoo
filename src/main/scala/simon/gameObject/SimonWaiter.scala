package simon
package gameObject

import
	org.wahoo.common._,
	org.wahoo.gameObject.Sprite

/** Waits for the player to press start. */
class SimonWaiter(pos:Vec)
	extends Sprite(pos)
	with ListensForStart
{
	// The animation will be in resources/animation/simon/gameObject/SimonWaiter
	def AniIdle =
		ani("idle")

	def onStart()
	{
		playSound("pressed")
		the[SimonMaster].done()
	}
}
