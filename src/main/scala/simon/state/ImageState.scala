package simon
package state

import
	org.wahoo.common._,
	org.wahoo.gameObject.GameObject,
	org.wahoo.gameObject.graphic.NamedBackground,
	org.wahoo.shape.Rect,
	org.wahoo.state.GameState,
	org.wahoo.util.packageToDirectoryName

/** A state that shows an image in the background. */
abstract class ImageState
	extends GameState
{
	def AnimationDirectoryName =
		getClass.getSimpleName

	def AnimationFullDirectory =
		s"${packageToDirectoryName(getClass)}/$AnimationDirectoryName"

	def initObjects: Seq[GameObject] =
		Seq(new NamedBackground(AnimationFullDirectory))

	def ArenaSize =
		Vec(1024, 1024)
}
