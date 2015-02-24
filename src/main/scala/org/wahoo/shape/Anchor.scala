package org.wahoo
package shape

object Anchor {
	case object CenterLeft extends Anchor
	case object CenterRight extends Anchor
	case object CenterBottom extends Anchor
	case object CenterTop extends Anchor
	case object Center extends Anchor
	case object LeftBottom extends Anchor
	case object RightBottom extends Anchor
	case object LeftTop extends Anchor
	case object RightTop extends Anchor

	val AllAnchors = Seq(
		CenterLeft, CenterRight, CenterBottom, CenterTop,
		Center, LeftBottom, RightBottom, LeftTop, RightTop)
}

/** Contains values representing the nine points all shapes can be anchored by.
	* A shape is anchored at a point if size changes leave that point constant.
	* Note that "anchoring" a shape does not prevent it from moving, and all
	* setSize operations must be given an anchor.
	*/
sealed abstract class Anchor
