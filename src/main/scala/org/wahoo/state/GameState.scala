package org.wahoo
package state

import
	collection.{ArrayBag, Bag, SortedBag},
	color.Color,
	gameObject.{GameObject, InArena, StepsOffscreen, Unique},
	gameObject.camera.{Camera, SimpleCamera},
	gameObject.light.LightTracker,
	math.implicits._,
	math.Scalar,
	shape.{Rect, Shape, Vec},
	spacialStore.SpacialStore

import
	org.lwjgl.opengl.GL11,
	scala.reflect.classTag

object GameState
{
	var state: GameState = null
	var camera: Camera = null
	var lightTracker: LightTracker = null
	var spacialStore: SpacialStore = null
}

abstract class GameState
	extends MinimalGameState
	with StoresByType
	with StoresUniques
{
	def initObjects: Seq[GameObject]

	/** Area where camera can look at. */
	def CameraRegion =
		Rect(Vec.Zero, ArenaSize)

	/** Size of the full gameplay arena. */
	def ArenaSize: Vec

	def StartAmbientLight =
		Color.White

	def MinSpacialStoreNodeSize: Scalar =
		MaxSpacialStoreNodeSize

	def MaxSpacialStoreNodeSize: Scalar =
		(ArenaSize.x max ArenaSize.y) / 2

	val ArenaShape: Shape =
		Rect(ArenaSize)

	def cameraSize =
		main.WindowSize

	def windowTitle = main.title
		// For debug:
		// s"${main.title} @${main.measuredFPS}FPS state:$getClass "+
		// s"pos:${camera.shape.center.intString}"

	var time = 0

	def StoreAllObjects =
		Settings.SpacialStoreLessStep || Settings.SpacialStoreLessDraw

	// not needed unless Settings.SpacialStoreLessDraw
	val objects =
		if (StoreAllObjects)
			SortedBag.empty[GameObject](
					classTag[GameObject],
					GameObject.DrawOrdering)
		else
			null

	val stepsOffscreens =
		Bag.empty[StepsOffscreen]

	val objectsNotInArenaDrawBefore =
		SortedBag.empty[GameObject](
			classTag[GameObject],
			GameObject.DrawOrdering)

	val objectsNotInArenaDrawAfter =
		SortedBag.empty[GameObject](
			classTag[GameObject],
			GameObject.DrawOrdering)

	val objectsToRemove =
		ArrayBag.empty[GameObject]

	var spacialStore: SpacialStore = null

	def StartCamera: Camera =
		new SimpleCamera(CameraRegion)

	var camera: Camera = null

	def LightNodeSize: Scalar =
		16

	var lightTracker: LightTracker = null

	/** Size of the area around the camera to be updated each frame.
		* This does not apply to objects that are StepsOffscreen.
		*/
	val UpdateAreaSize =
		cameraSize * 1.2

	/** Screen is set to this color before any drawing takes place. */
	def backgroundColor =
		Color.Black

	def start()
	{
		GameState.state = this

		spacialStore =
			SpacialStore(
				ArenaSize,
				MinSpacialStoreNodeSize,
				MaxSpacialStoreNodeSize)
		GameState.spacialStore = spacialStore

		lightTracker =
			new LightTracker(StartAmbientLight, LightNodeSize)
		GameState.lightTracker = lightTracker

		camera =
			StartCamera
		GameState.camera = camera

		add(camera)
		add(lightTracker)

		for (obj <- initObjects)
			add(obj)
	}

	def restart()
	{
		GameState.state = this
		GameState.spacialStore = spacialStore
		GameState.lightTracker = lightTracker
		GameState.camera = camera
	}

	def pause()
	{

	}

	def finish()
	{
		pause()
	}

	def step()
	{
		time += 1

		if (Settings.SpacialStoreLessStep)
			for (obj <- objects)
				obj.step()
		else {
			for (obj <- objectsNotInArenaDrawBefore)
				obj.step()
			for (obj <- objectsNotInArenaDrawAfter)
				obj.step()
			for (obj <- stepsOffscreens)
				obj.step()
			//Only update objects near the camera.
			val bounds = camera.boundary withSize UpdateAreaSize
			spacialStore callAllStepsColliding bounds
		}

		deleteAllKilled()
	}

	/** Projection for all objects that don't follow the camera.
		* Orthographic, size of the window, origin at left-bottom.
		*/
	def setupPreCameraProjection()
	{
		GL11.glLoadIdentity()
		GL11.glOrtho(
			0, cameraSize.x.toDouble,
			0, cameraSize.y.toDouble,
			-1, 1)
	}

	def draw()
	{
		backgroundColor.applyGLClearColor()
		GL11.glClear(GL11.GL_COLOR_BUFFER_BIT)

		setupPreCameraProjection()

		for (obj <- objectsNotInArenaDrawBefore)
			obj.draw()

		camera.applyProjection()

		if (Settings.SpacialStoreLessDraw)
			// Draw every object (SLOW)
			for (obj <- objects)
				obj.draw()
		else
			spacialStore drawAllColliding camera.boundary

		if (Settings.DrawSpacialStore)
			spacialStore drawNodesColliding camera.boundary

		setupPreCameraProjection()

		for (obj <- objectsNotInArenaDrawAfter)
			obj.draw()
	}

	def add(go:GameObject)
	{
		if (go == null)
			throw new NullPointerException()
		if (StoreAllObjects)
			objects += go

		addToStepsOffscreens(go)
		addToUniques(go)
		addToByType(go)
		addToSpacialStore(go)

		go.start()
	}

	def addToSpacialStore(go:GameObject)
	{
		go match {
			case ia:InArena =>
				spacialStore add ia
			case nia if (nia.DrawOrder < 0) =>
				objectsNotInArenaDrawBefore += nia
			case nia =>
				objectsNotInArenaDrawAfter += nia
		}
	}

	def removeFromSpacialStore(go:GameObject) {
		go match {
			case ia:InArena =>
				// it will remove itself
			case nia if (nia.DrawOrder < 0) =>
				objectsNotInArenaDrawBefore -= nia
			case nia =>
				objectsNotInArenaDrawAfter -= nia
		}
	}

	def loseObject(go:GameObject)
	{
		objectsToRemove += go
	}

	def deleteAllKilled()
	{
		if (StoreAllObjects)
			objects --= objectsToRemove

		for (obj <- objectsToRemove) {
			removeFromUniques(obj)
			removeFromByType(obj)
			removeFromSpacialStore(obj)
			removeFromStepsOffscreens(obj)

			obj.finish()
		}

		objectsToRemove.clear()
	}

	def addToStepsOffscreens(go:GameObject)
	{
		go match {
			case s:StepsOffscreen =>
				stepsOffscreens += s
			case _ =>
				// do nothing
		}
	}

	def removeFromStepsOffscreens(go:GameObject)
	{
		go match
		{
			case s:StepsOffscreen =>
				stepsOffscreens -= s
			case _ =>
				// do nothing
		}
	}
}
