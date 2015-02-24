package org.wahoo
package state

import
	gameObject.GameObject,
	gameObject.camera.Camera,
	gameObject.control.{Controller, KeyboardController},
	gameObject.loader.SynchronizedAnimationMaster,
	math.implicits._,
	math.Scalar,
	shape.{Rect, Vec}

import scala.collection.mutable

abstract class Level extends GameState
{
	def MapGrid: String

	/** How large (in stage units) a single square of the map is. */
	def MapSquareSize: Scalar

	/** Number of squares in the map, left to right and bottom to top. */
	def MapSize: (Int, Int)

	/** This many squares around the border will be inaccessible to the camera. */
	def CameraBorder  : (Int, Int)

	/** This many additional squares will be added around the map.
		* The camera region will not include the padding.
		* Padding is highly recommended!
		*/
	def MapPadding    : (Int, Int)

	private def mapFullWidth : Int =
		MapSize._1 + MapPadding._1 * 2

	private def mapFullHeight: Int =
		MapSize._2 + MapPadding._2 * 2

	private def fullCameraBorder: (Int, Int) =
		(MapPadding._1 + CameraBorder._1,
			MapPadding._2 + CameraBorder._2)

	def ArenaSize =
		Vec(
			mapFullWidth .nextPowerOf2 * MapSquareSize,
			mapFullHeight.nextPowerOf2 * MapSquareSize)

	override def CameraRegion =
		Rect(
			Vec(fullCameraBorder) * MapSquareSize,
			Vec(
				mapFullWidth - fullCameraBorder._1*2,
				mapFullHeight - fullCameraBorder._2*2) * MapSquareSize)

	override def MinSpacialStoreNodeSize =
		MapSquareSize * 2

	override def MaxSpacialStoreNodeSize =
		MapSquareSize * 8

	def initObjects =
		LevelDefaultObjects ++ AdditionalObjects ++ mapToObjects(MapGrid)

	private def LevelDefaultObjects =
		Seq(controller, new SynchronizedAnimationMaster)

	def AdditionalObjects: Seq[GameObject]

	def controller: Controller =
		KeyboardController()

	var codes =
		mutable.Map[Char, Vec => Iterable[GameObject]]()

	def emptyCode(name:Char)
	{
		code(name) { v => }
	}

	def emptyCodes(names:Char*)
	{
		for (name <- names)
			emptyCode(name)
	}

	def code(name:Char)(f:Vec => Any)
	{
		require(!codes.contains(name), s"Code $name used twice")

		codes(name) = { v:Vec =>
			f(v) match {
				case () =>
					None
				case g:GameObject =>
					Some(g)
				case i:Iterable[_] =>
					i.asInstanceOf[Iterable[GameObject]]
			}
		}
	}


	private def mapToObjects(str:String): Seq[GameObject] =
	{
		val lines = str.lines.dropWhile(_.isEmpty).toSeq
		val nRows = lines.size

		assert(nRows == MapSize._2,
			s"MapGrid should have ${MapSize._2} rows, not $nRows")

		for (
			(row, rowNumber) <- lines.zipWithIndex;
			codes = row.slice(0, row.size - 1); //Take out final '`'
			_ = require(codes.size == MapSize._1,
				s"Row $row corrupt in MapGrid. " +
				s"It should have size ${MapSize._1}, not ${codes.size}.");
			(code, colNumber) <- codes.zipWithIndex;
			obj <- objectsFromCode(colNumber, rowNumber, code))
			yield obj
	}

	private def objectsFromCode(mapX:Int, mapY:Int, code:Char): Iterable[GameObject] =
	{
		if (code == ' ')
			None
		else {
			//String is read top to bottom, but objects are put down bottom to top.
			//So must invert y.

			val idx =
				(mapX + MapPadding._1,
				MapSize._2 - 1 - mapY + MapPadding._2)

			//Use centers, not corners.
			val idxCenter =
				Vec(idx) + Vec(0.5, 0.5)

			val vec =
				idxCenter * MapSquareSize

			if (!(codes contains code))
				throw new RuntimeException(
					s"Undefined code $code used in map")

			codes(code)(vec)
		}
	}
}
