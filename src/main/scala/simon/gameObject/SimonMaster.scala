package simon
package gameObject

//import
//	state.Win

import
	org.wahoo.common._,
	org.wahoo.gameObject.{GameObject, Unique},
	org.wahoo.gameObject.camera.Camera,
	org.wahoo.shape.Side,
	org.wahoo.math.randomOf

import scala.collection.mutable

/** Transitions between phases of the game.
  * The game:
  * ~ Waits for the player to press Start.
  * ~ Shows the player arrows and waits for the player to match them.
  * ~ Waits for the player to press Start.
  * ~ Expects the player to enter the same input again.
  */
class SimonMaster
	extends GameObject
	with Unique
{
	def ApplaudEveryNScores =
		5

	val counter =
		// `export` creates an object and adds it to the game.
		export(new ScoreCounter(0))

	/** We will add on to the end of this after every round.
	  * A Side is Left, Right, Bottom, or Top;
	  * these correspond to the inputs we want.
	  */
	val data =
		new mutable.ArrayBuffer[Side]()

	/** The score is how big the list of inputs is.
	  * We will have to manually keep the counter up-to-date.
	  */
	def score =
		data.size

	/** A state that starts by exporting a helper and ends by killing it. */
	sealed trait HelperState extends State
	{
		def helper: GameObject
	}
	case class Listening(helper:SimonListener) extends HelperState
	case class Saying(helper:SimonSayer) extends HelperState
	case class WaitToListen(helper:SimonWaiter) extends HelperState
	case class WaitToSay(helper:SimonWaiter) extends HelperState

	override def StartState =
		WaitToSay(waiter())

	def waiter() =
		new SimonWaiter(the[Camera].center)

	// Called every time a state is begun.
	override def begin(state:State)
	{
		super.begin(state)

		state match {
			case h:HelperState =>
				export(h.helper)
			case _ =>
		}
	}

	/** Called by a helper when it has finished. */
	def done()
	{
		val asHelper =
			state.asInstanceOf[HelperState]

		asHelper.helper.die()

		asHelper match
		{
			case _:Listening =>
				be(WaitToSay(waiter()))

			case _:Saying =>
				be(WaitToListen(waiter()))

			case _:WaitToSay =>
				addNew()
				be(Saying(new SimonSayer(data)))

			case _:WaitToListen =>
				be(Listening(new SimonListener(data)))
		}
	}

	def addNew()
	{
		data += randomOf(Side.All)

		counter.score = score

		if (score divisibleBy ApplaudEveryNScores)
			playSound(s"levelUp")
	}
}
