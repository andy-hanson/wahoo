package org.wahoo

import
	math.implicits._,
	shape.{Rect, Vec},
	state.GameState

import
	java.io.IOException,
	org.lwjgl.opengl.{Display, DisplayMode, GL11},
	scala.collection.mutable.Stack

/** Stores a link to [[org.wahoo.WahooMain]] if the game state needs to access it.
  * So far that has never been necessary.
  */
object WahooMain
{
	var main:WahooMain = null
}

/** Runs the entire game.
  * Classes that override WahooMain must define an initial [[org.wahoo.state.GameState]].
  * [[org.wahoo.state.GameState]]s can request main to push a new GameState on top of them
  * or to be popped off.
  */
abstract class WahooMain
{

	/** The [[org.wahoo.state.GameState]] that starts the game. */
	def StartGameState: GameState

	/** Title of your game. */
	def title: String

	protected def WindowDimension: (Int, Int)

	/** (windowWidth, windowHeight) */
	def WindowSize =
		Vec(WindowDimension._1, WindowDimension._2)

	/** How many times a second your game should step and draw.
		* The only guarantee is that the game won't run faster than this.
		*/
	def DesiredFPS =
		30

	/** The current measured framerate. */
	var measuredFPS =
		0

	/** How long (in milliseconds) to wait while measuring measuredFPS */
	val FPSUpdateMSecs =
		2000

	/** The last clock time (in milliseconds) that measuredFPS was measured. */
	var lastUpdateClock:Long =
		0

	/** The number of frames this game has been running. */
	var time =
		0

	/** The last time (in frames) that measuredFPS was measured. */
	var lastUpdateTime =
		0

	/** The current [[org.wahoo.state.GameState]]
		* @todo replace with an argument to runLoop()
		*/
	var gameState:GameState = null // Written to later on in constructor

	/** Stack of [[org.wahoo.state.GameState]]s to play.
	  * A state on the bottom is started first and ended last.
	  * The head is the current state.
	  * Ex. The title screen is returned to on game over.
	  */
	var gameStates =
		Stack[GameState]()

	var mustPushState: Option[GameState] = None

	def pushState(state:GameState)
	{
		mustPushState = Some(state)
	}

	/** Starts playing a new [[org.wahoo.state.GameState]].
		* The old state will resume. playing when the new one exits.
		*/
	def actuallyPushState(state:GameState)
	{
		if (gameState != null)
			gameState.pause()

		gameState = state
		gameStates push gameState

		gameState.start()
	}

	var mustPopState: Boolean =
		false

	def popState()
	{
		mustPopState = true
	}

	/** End the current [[org.wahoo.state.GameState]].
	  * If there was a state pushed before this, game continues from there.
	  * If there was no other GameState, calls endGame() and exits.
	  */
	def actuallyPopState()
	{
		val popped = gameStates.pop()
		assert(popped == gameState)

		if (gameStates.isEmpty)
			done ||= mustPushState.isEmpty
		else {
			gameState = gameStates.last
			gameState.restart()
		}
	}

	/** End the current [[org.wahoo.state.GameState]] and start a new one.
	  * A push and pop at once.
	  */
	def replaceState(state:GameState)
	{
		popState()
		pushState(state)
	}

	/** Run the game. */
	def play()
	{
		start()
		prepareGame()
		runLoop()
		finish()
	}

	/** Called before the game begins. */
	def start() { }

	/** Called after the game ends. */
	def finish() { }

	/** Sets up everything for the main loop. */
	def prepareGame()
	{
		WahooMain.main = this

		// Note: OpenGL must be initialized before images can be loaded.
		initGL()

		actuallyPushState(StartGameState)
	}

	private var done = false

	/** Executes the main game loop.
	  * Calls upon [[org.wahoo.state.GameState]]'s step and draw methods.
	  * Also keeps track of (real) time, sets the window title, and gets input.
	  */
	private def runLoop()
	{
		while (!done)
		{
			gameState.step()
			gameState.draw()
			Display.update()
			Display setTitle gameState.windowTitle

			checkFPS()

			Display sync DesiredFPS

			if (mustPopState) {
				mustPopState = false
				actuallyPopState()
			}

			for (state <- mustPushState) {
				mustPushState = None
				actuallyPushState(state)
			}

			done ||= Display.isCloseRequested()
		}

		onEnd()
	}

	/** Close everything.
	  */
	def onEnd()
	{
		println("Good-bye")
		Display.destroy()
	}

	/** Checks the time and uses this to calculate the FPS.
	  * This function only reads it and does not sleep.
	  * Display.sync(FPS) will do that.
	  */
	private def checkFPS()
	{
		time += 1
		val clock = System.currentTimeMillis()

		if (clock > lastUpdateClock + FPSUpdateMSecs) {
			measuredFPS = (time - lastUpdateTime) * 1000 / FPSUpdateMSecs
			lastUpdateTime = time
			lastUpdateClock = clock
		}
	}

	/** One-time openGL settings upon game start.
		* @todo merge with Camera#setupDraw
		*/
	private def initGL()
	{
		import GL11._

		Display setDisplayMode
			new DisplayMode(WindowDimension._1, WindowDimension._2)
		Display.create()
		Display setVSyncEnabled true
		// Display.setIcon

		glEnable(GL_TEXTURE_2D)
		glEnable(GL_LINE_SMOOTH)

		// Enable alpha blending.
		glEnable(GL_BLEND)
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

		// No 3d stuff needed.
		glDisable(GL_DEPTH_TEST)

		// Will only ever use projection matrix.
		glMatrixMode(GL_PROJECTION)
	}
}
