package org.wahoo
package gameObject

import
	loader.{LoadsAnimations, LoadsSounds},
	logic.{AccessesClassMap, AccessesUniques, HasState},
	logger.Logs,
	state.GameState

object GameObject
{
	/*
	private var nextID = 0
	def getNextID() =
	{
		nextID += 1;
		nextID
	}
	*/

	val DrawOrdering: Ordering[GameObject] = Ordering.by(_.DrawOrder)
	//val DrawOrderingUnique: Ordering[GameObject] = Ordering.by(_.ID)
}

trait GameObject
	extends AccessesClassMap
	with AccessesUniques
	with HasState
	with LoadsAnimations
	// with Logs
	with LoadsSounds
{
	/** Objects with low drawOrder draw earliest and appear in the back.
	  * Objects with high drawOrder draw latest and appear in the front.
	  */
	def DrawOrder = 0

	/** Unique identifier. Makes tree storage more efficient.
	  * Use 8 bits drawOrder, then 24 bits of a unique code.
	  */
	//val ID = drawOrder << 24 | GameObject.getNextID()

	case object Dead extends State

	def begin(s:State)
	{
		s match {
			case Dead => gameState.loseObject(this)
			case _    =>
		}
	}

	def end(s:State)
	{
		require(s != Dead, "Not allowed to rise from the grave!")
	}

	def alive =
		not(Dead)
	def dead =
		am(Dead)

	/** Advance's this object's game logic by a single frame.
		* Objects of type HasNode will only update if they are near the camera.
		*/
	def step() { }

	/** Renders this object onscreen.
		* Usually, this will happen in the camera's translation.
		* Objects of type HasNode will only draw if they are onscreen.
		*/
	def draw() { }

	/** Marks this as dead, and to be cleaned up by GameState at end of frame.
		* Also calls onDie().
		* GameState will handle all actual deletion.
		*/
	def die()
	{
		be(Dead)
	}

	def StartState: State =
		Idle

	/** Anything this object needs to do when created.
	  */
	def start()
	{
		mustBegin(StartState)
	}

	/** Anything this object needs to do when killed.
		* This method will only be called once, even if die() is called multiple times.
		* Usually unneeded unless something needs to happen *no matter how* an object dies.
		* Consider ex. using badGuy.blowUp() rather than badGuy.die().
		*/
	def finish() { }

	/** The current game state. */
	def gameState =
		GameState.state

	/** The current game state's camera. */
	def camera =
	{
		val c = GameState.camera
		require(c == gameState.camera)
		c
	}

	/** The current game state's light tracker. */
	def lightTracker =
	{
		val l = GameState.lightTracker
		require(l == gameState.lightTracker)
		l
	}

	/** The current game state's spacial store. */
	def spacialStore =
	{
		val s = GameState.spacialStore
		require(s == gameState.spacialStore)
		s
	}

	/** Create a new object and send it to the GameState.
	  * Also returns that object.
	  */
	def export[A <: GameObject](a:A) =
	{
		gameState add a
		a
	}
}

