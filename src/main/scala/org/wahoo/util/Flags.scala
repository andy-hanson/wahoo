package org.wahoo
package util

object Flags
{
	def apply(): Flags =
		apply(0)
	def apply(keys:Seq[Int]): Flags =
		apply(bitsOfSet(keys))

	/** All 0s except for nth bit from little end. */
	private def bit(n:Int) =
	{
		require(0 <= n && n < 32)
		1 << n
	}

	/** All 1s except for nth bit from little end. */
	private def unbit(n:Int) =
		~bit(n)

	/** For each number in keys, that bit is on. */
	def bitsOfSet(keys:TraversableOnce[Int]) =
	{
		var bits = 0
		for (key <- keys)
			bits |= 1 << key
		bits
	}

	/** For each number in keys, that bit is off. */
	private def unBitsOfSet(keys:TraversableOnce[Int]) =
		~bitsOfSet(keys)
}

/** Keeps track of Boolean values (called flags) indexed from 0 to 31.
	*/
case class Flags(private var bits:Int)
{
	import Flags._

	/** Whether the flag at the index is up. */
	def apply(n:Int) =
		(bits & bit(n)) != 0

	/** Set a flag up or down depending on the boolean. */
	def update(n:Int, b:Boolean)
	{
		if (b)
			on(n)
		else
			off(n)
	}

	/** Put a flag up. Now this(n) == true. */
	def on(n:Int)
	{
		bits |= bit(n)
	}

	/** Take a flag down. Now this(n) == false. */
	def off(n:Int)
	{
		bits &= unbit(n)
	}

	/** Set multiple flags up at once. */
	def on(n:Seq[Int])
	{
		bits |= bitsOfSet(n)
	}

	/** Take multiple flags down at once. */
	def off(n:Seq[Int])
	{
		bits &= unBitsOfSet(n)
	}

	/** Convert the flags to a sequence.
		* This function is slow and should only be used for debugging.
		*/
	def toSeq =
	{
		var s = Seq[Int]()
		each { it =>
			s = s :+ it
		}
		s
	}

	/** Iterate through all flags that are up.
		* Guaranteed in order from least index to greatest index.
		* This function is slow and should only be used for debugging.
		*/
	def each(f:Int => Unit)
	{
		var shiftedBits = bits
		var i = 0
		while (shiftedBits != 0) {
			if ((shiftedBits & 1) != 0)
				f(i)
			i += 1
			shiftedBits >>>= 1
		}
	}

	/** Whether every flag in a set is up. */
	def all   (keys:TraversableOnce[Int]) =
		(bitsOfSet(keys) & ~bits) == 0

	/** Whether at least one flag in a set is down. */
	def notAll(keys:TraversableOnce[Int]) =
		!all(keys)

	/** Whether at least one flag in a set is up. */
	def any   (keys:TraversableOnce[Int]) =
		!none(keys)

	/** Whether every flag in a sequence is down. */
	def none  (keys:TraversableOnce[Int]) =
		(bitsOfSet(keys) & bits) == 0

	/** Whether every flag is down. */
	def none =
		bits == 0

	/** Whether every flag is up. */
	def any =
		!none
}
