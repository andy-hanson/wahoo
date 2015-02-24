package org.wahoo

import
	color.Color,
	math.implicits._,
	math.Scalar,
	resource.{Animation, Image, NoImage, RasterImage},
	shape.{Shape, Vec, Rect, Circle},
	state.GameState

import org.lwjgl.opengl.GL11._

package object render
{
	private var textureEnabled = false

	def checkTextureEnabled()
	{
		if (!textureEnabled) {
			glEnable(GL_TEXTURE_2D)
			textureEnabled = true
		}
	}

	def checkTextureDisabled()
	{
		if (textureEnabled) {
			glDisable(GL_TEXTURE_2D)
			textureEnabled = false
		}
	}

	private def white()
	{
		glColor3ub(255.toByte, 255.toByte, 255.toByte)
	}

	private def prepareShape(lineWidth:Scalar, color:Color)
	{
		checkTextureDisabled()
		glLineWidth(lineWidth.toFloat)
		color.applyGL()
	}

	private def vertex(v:Vec)
	{
		vertex(v.x, v.y)
	}

	private def vertex(x:Scalar, y:Scalar)
	{
		glVertex2d(x.toFloat, y.toFloat)
	}

	private def glMode(mode:Int)(done: => Unit)
	{
		glBegin(mode)
		done
		glEnd()
	}

	def manyVecs(f: => Unit)
	{
		checkTextureDisabled()
		glMode(GL_POINTS)(f)
	}

	def preparedVec(vec:Vec, color:Color)
	{
		color.applyGL()
		vertex(vec)
	}

	def vec(v:Vec, lineWidth:Scalar, color:Color)
	{
		vec(v.x, v.y, lineWidth, color)
	}

	def vec(x:Scalar, y:Scalar, lineWidth:Scalar, color:Color)
	{
		checkTextureDisabled()
		prepareShape(lineWidth, color)
		glMode(GL_POINTS) {
			vertex(x, y)
		}
	}

	def vec(v:Vec, color:Color)
	{
		vec(v.x, v.y, color)
	}

	def vec(x:Scalar, y:Scalar, color:Color)
	{
		checkTextureDisabled()
		color.applyGL()
		glMode(GL_POINTS) {
			vertex(x, y)
		}
	}

	def rect(r:Rect, lineWidth:Scalar, color:Color)
	{
		prepareShape(lineWidth, color)
		glMode(GL_LINE_LOOP) {
			vertex(r.leftBottom)
			vertex(r.rightBottom)
			vertex(r.rightTop)
			vertex(r.leftTop)
		}
	}

	def circle(c:Circle, lineWidth:Scalar, color:Color)
	{
		prepareShape(lineWidth, color)
		val N = 16
		val dang = 2 * math.Pi / N
		glMode(GL_LINE_SMOOTH) {
			var angle:Scalar = 0
			N.times {
				vertex(angle.cos, angle.sin)
				angle += dang
			}
		}
	}

	/** Draws this Shape using OpenGL functions.
	  * Only draws lines and points.
	  * Does not choose color or line width.
	  */
	def shape(s:Shape, lineWidth:Scalar, color:Color) {
		//println("Drawing shape "+s+" with width "+lineWidth+" and color "+color)
		s match {
			case v:Vec =>
				vec(v, lineWidth, color)
			case r:Rect =>
				rect(r, lineWidth, color)
			case c:Circle =>
				circle(c, lineWidth, color)
		}
	}

	def animation(
		ani:Animation,
		shape:Shape,
		lit:Boolean,
		flipX:Boolean = false,
		flipY:Boolean = false)
	{
		animation(ani, shape.centerX, shape.centerY, lit, flipX, flipY)
	}

	def animation(
		ani:Animation,
		x:Scalar,
		y:Scalar,
		lit:Boolean,
		flipX:Boolean,
		flipY:Boolean)
	{
		image(ani.frame, x, y, lit, flipX, flipY)
	}

	def animation(ani:Animation, x:Scalar, y:Scalar, lit:Boolean)
	{
		animation(ani, x, y, lit, false, false)
	}

	def image(
		img:Image,
		shape:Shape,
		lit:Boolean,
		flipX:Boolean,
		flipY:Boolean)
	{
		//shape match {
		//  case r:Rect => image.drawLeftBottom(r.leftBottom)
		//  case _      => image.drawCentered(shape.center)
		//}
		image(img, shape.centerX, shape.centerY, lit, flipX, flipY)
	}

	def image(
		img:Image,
		x:Scalar,
		y:Scalar,
		lit:Boolean,
		flipX:Boolean,
		flipY:Boolean)
	{
		img match {
			case r:RasterImage =>
				if (lit) litRaster(r, x, y, flipX, flipY)
				else        raster(r, x, y, flipX, flipY)
			case NoImage => /* do nothing */
		}
	}

	def raster(
		img:RasterImage,
		x:Scalar,
		y:Scalar,
		flipX:Boolean,
		flipY:Boolean)
	{
		checkTextureEnabled()

		white()

		img.texture.bind()
		if (img.hardScale)
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)

		val (x0, x1) =
			if (flipX)
				(x + img.halfWidth , x - img.halfWidth)
			else
				(x - img.halfWidth , x + img.halfWidth)
		val (y0, y1) =
			if (flipY)
				(y + img.halfHeight, y - img.halfHeight)
			else
				(y - img.halfHeight, y + img.halfHeight)

		glMode(GL_QUADS) {
			glTexCoord2d(0, img.heightFrac)
			vertex(x0, y0)

			glTexCoord2d(img.widthFrac, img.heightFrac)
			vertex(x1, y0)

			glTexCoord2d(img.widthFrac, 0)
			vertex(x1, y1)

			glTexCoord2d(0, 0)
			vertex(x0, y1)
		}
	}

	//@todo: break down image into tiles
	def litRaster(
		img:RasterImage,
		x:Scalar,
		y:Scalar,
		flipX:Boolean,
		flipY:Boolean)
	{
		val lt = GameState.lightTracker

		checkTextureEnabled()

		img.texture.bind()
		if (img.hardScale)
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)

		val (x0, x1) =
			if (flipX)
				(x + img.halfWidth , x - img.halfWidth)
			else
				(x - img.halfWidth , x + img.halfWidth)
		val (y0, y1) =
			if (flipY)
				(y + img.halfHeight, y - img.halfHeight)
			else
				(y - img.halfHeight, y + img.halfHeight)

		//@todo prettify code
		glMode(GL_QUADS) {
			lt.colorAt(x0, y0).applyGL()
			glTexCoord2d(0, img.heightFrac)
			vertex(x0, y0)

			lt.colorAt(x1, y0).applyGL()
			glTexCoord2d(img.widthFrac, img.heightFrac)
			vertex(x1, y0)

			lt.colorAt(x1, y1).applyGL()
			glTexCoord2d(img.widthFrac, 0)
			vertex(x1, y1)

			lt.colorAt(x0, y1).applyGL()
			glTexCoord2d(0, 0)
			vertex(x0, y1)
		}
	}

	/*def litRaster(img:RasterImage,  x:Scalar, y:Scalar,
								flipX:Boolean, flipY:Boolean) {
		val lt = GameState.lightTracker

		checkTextureEnabled()

		img.texture.bind()
		if (img.hardScale) glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)

		val (x0, x1) =
			if (flipX) (x + img.halfWidth , x - img.halfWidth)
			else       (x - img.halfWidth , x + img.halfWidth)
		val (y0, y1) =
			if (flipY) (y + img.halfHeight, y - img.halfHeight)
			else       (y - img.halfHeight, y + img.halfHeight)

		lt.matrix.something { (color, partOfTexture) {
			//glTexCoord2d(partOfTexture.left, partOfTexture.bottom)

			//EDIT!
			partOfTexture.eachCorner { corner =>
				light apply
				glTexCoord2d(corner - offset)
				vertex(corner)
			}
		}
	}*/

	///** Draw an image rotated around its center. */
	//def imageRotated(img:Image, cx:Double, cy:Double, radians:Double)

}
