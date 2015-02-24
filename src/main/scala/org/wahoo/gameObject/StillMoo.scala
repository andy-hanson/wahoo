package org.wahoo
package gameObject

import shape.Vec

/** Class for objects that never move.
  * Avoids keeping track of velocity and whatnot.
  */
abstract class StillMoo(initPos:Vec)
	extends Sprite(initPos)
	with InArenaAndMutableShape { }
