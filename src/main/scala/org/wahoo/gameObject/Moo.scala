package org.wahoo
package gameObject

import shape.Vec

abstract class Moo(initPos:Vec)
	extends Sprite(initPos)
	with InArenaAndMutableShape
	with Moves
