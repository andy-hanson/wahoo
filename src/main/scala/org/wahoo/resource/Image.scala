package org.wahoo
package resource

import
	math.Scalar,
	math.implicits._,
	shape.Vec

import
	org.newdawn.slick.opengl.{Texture, TextureLoader},
	org.newdawn.slick.util.ResourceLoader,
	org.lwjgl.opengl.GL11._

/** Factory for getting Images. */
object Image
{
	val FileType =
		"png"

	/** Load a raster image from a file name.
		* Image loading is not memoized, but Animation loading is.
		*/
	def apply(fileName:String, scale:Scalar) =
	{

		val path =
			s"$fileName.$FileType"
		// println(s"LOADING $path")
		val stream =
			ResourceLoader getResourceAsStream path
		val texture =
			TextureLoader getTexture (FileType, stream)

		RasterImage(fileName, scale, texture)
	}
}

sealed abstract class Image
{
	/** Number of columns in the picture (not the texture). */
	def width: Int

	/** Number of rows in the picture (not the texture). */
	def height: Int

	/** Size of the picture (not the texture). */
	def size: Vec
}

/** Immutable picture. Knows how to draw itself.
  * Fastest drawing through drawLeftBottom.
  * Rotation available through drawRotated.
  * @todo drawLit, drawLitRotated
  */
case class RasterImage private[resource](
	fileName: String,
	scale: Scalar,
	texture: Texture)
	extends Image
{

	val width  =
		(texture.getImageWidth  * scale).toInt

	val height =
		(texture.getImageHeight * scale).toInt

	val size =
		Vec(width, height)

	/** Number of columns in texture representation. */
	private val textureWidth =
		texture.getTextureWidth

	/** Number of rows in texture representation. */
	private val textureHeight =
		texture.getTextureHeight

	/** Size of texture representation. */
	//private val textureSize = Vec(textureWidth, textureHeight)
	/** OpenGL ID of this texture.
		* @todo check if this is really necessary to have.
		*/
	//private val textureID = texture.getTextureID

	val texWidth =
		textureWidth * scale
	val texHeight =
		textureHeight * scale

	/** Fraction of the texture columns the actual picture takes up. */
	val widthFrac: Double =
		texture.getImageWidth.toDouble  / textureWidth
	/** Fraction of the texture rows the actural picture takes up. */
	val heightFrac: Double =
		texture.getImageHeight.toDouble / textureHeight

	/** Half of picture width. */
	val halfWidth =
		(texture.getImageWidth  * scale / 2).toInt
	/** Half of picture height. */
	val halfHeight =
		(texture.getImageHeight * scale / 2).toInt

	/** If turned on, scaling will not use smoothing. */
	def hardScale =
		true
}

case object NoImage extends Image
{
	val width = 0
	val height = 0
	val size = Vec.Zero
}
