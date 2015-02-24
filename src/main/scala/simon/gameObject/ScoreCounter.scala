package simon
package gameObject

import
	org.wahoo.common._,
	org.wahoo.gameObject.{GameObject, Unique},
	org.wahoo.math.toDigits,
	org.wahoo.resource.Animation

/** Displays the score. */
class ScoreCounter(
	var score: Int)
	extends GameObject
	with Unique
{
	/** How many digits to display. */
	def NDigits =
		2

	/** Horizontal distance between the left side of each digit. */
	def DigitSpacing =
		64

	val DigitAnis: Array[Animation] =
		(for (i <- 0 to 9)
			yield animation("#"+i)
		).toArray


	override def draw()
	{
		val margin =
			48
		val y =
			gameState.cameraSize.y - margin

		drawNumber(score, margin, y)
	}

	def drawNumber(amount:Int, leftX:Scalar, y:Scalar)
	{
		// Don't display more than we've allowed for.
		val displayAmount =
			amount min (10 ** NDigits - 1)

		for (
			(digit, index) <- toDigits(displayAmount, NDigits).zipWithIndex;
			ani = DigitAnis(digit);
			x = leftX + DigitSpacing * index
			)
			renderAnimation(ani, x, y)
	}
}
