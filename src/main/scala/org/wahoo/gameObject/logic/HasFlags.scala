package org.wahoo
package gameObject
package logic

import scala.collection.mutable

trait HasFlags
{
	abstract class Flag

	def begin(f:Flag) { }

	def end(f:Flag) { }

	def StartFlags: Set[Flag] =
		Set.empty[Flag]

	private var amFlags =
		mutable.Set(StartFlags.toSeq: _*)

	def assertOn (f:Flag)
	{
		assert(isOn(f))
	}
	def assertOff(f:Flag)
	{
		assert(isOff(f))
	}

	def mustTurnOn(f:Flag)
	{
		require(isOff(f), s"Trying to begin $f, but am already.")
		amFlags += f
		begin(f)
	}

	def mustTurnOff(f:Flag)
	{
		require(isOn(f), s"Trying to end $f, but am already not.")
		amFlags -= f
		end(f)
	}

	def turnOn (flag:Flag)
	{
		if (isOff(flag))
			mustTurnOn(flag)
	}

	def turnOff(flag:Flag)
	{
		if (isOn(flag))
			mustTurnOff(flag)
	}

	/** Raise multiple flags. be(A, B) is equivalent to be(A); be(B).*/
	def turnOn(flags:Flag*)
	{
		for (flag <- flags)
			turnOn(flag)
	}

	/** Lower multiple flags. stop(A, B) is equivalent to stop(A); stop(B). */
	def turnOff(flags:Flag*)
	{
		for (flag <- flags)
			turnOff(flag)
	}

	def isOn(flag:Flag) =
		amFlags contains flag

	def isOff(f:Flag) =
		!isOn(f)

	def inspectFlags: String =
		amFlags.toString

	def allFlags(flags:Flag*) =
		flags forall { isOn(_) }

	def notAllFlags(flags:Flag*) =
		flags exists { isOff(_) }

	def anyFlags(flags:Flag*) =
		flags exists { isOn (_) }

	def noFlags(flags:Flag*) =
		flags forall { isOff(_) }
}
