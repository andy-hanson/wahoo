package org.wahoo
package gameObject

/** Signifier trait for when an object needs to do things even when offscreen.
  * This trait only for objects InArena. Other objects will always update anyway.
  */
trait StepsOffscreen extends InArena { }
