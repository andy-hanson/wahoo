package simon
package gameObject

import
	org.wahoo.common._,
	org.wahoo.gameObject.{GameObject, Unique},
	org.wahoo.gameObject.camera.Camera,
	org.wahoo.math.toDigits,
	org.wahoo.resource.Animation,
	org.wahoo.shape.Side

/** Shows arrows and waits for player to match them with input.
  * Forgives errors.
  */
class SimonSayer(data:Seq[Side])
	extends SimonInput(data)
{
	/** There is an animation for each possible input. */
	val SideAnis: Map[Side, Animation] =
		(for (side <- Side.All)
			yield (side, animation(s"$side", scale = 1))
		).toMap

	override def draw()
	{
		super.draw()

		renderAnimation(SideAnis(currentSide), the[Camera].center)
	}

	override def onDone()
	{
		playSound("done")
	}
}
