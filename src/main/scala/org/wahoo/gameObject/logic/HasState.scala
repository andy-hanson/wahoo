package org.wahoo
package gameObject
package logic

trait HasState
{
	abstract class State

	case object Idle extends State

	private var _state: State = null

	def state =
		_state

	def begin(s:State)
	def end  (s:State)

	def mustBe(s:State)
	{
		assert(am(s))
	}

	def mustNotBe(s:State)
	{
		assert(not(s))
	}

	def mustBegin(s:State)
	{
		require(not(s), "Trying to begin "+s+", but am already.")
		end(_state)
		_state = s
		begin(_state)
	}

	def mustChange(was:State, now:State)
	{
		require(was != now)
		mustBe(was)
		mustBegin(now)
	}

	def flipState(a:State, b:State)
	{
		if (am(a))
			mustBegin(b)
		else {
			mustBe(b)
			mustBegin(a)
		}
	}

	def maybeChange(from:State, to:State) {
		require(from != to)
		if (am(from))
			mustBegin(to)
	}

	def be(s:State)
	{
		if (not(s))
			mustBegin(s)
	}

	def am(state:State) =
		_state == state

	def not(state:State) =
		!am(state)

	def amAny (states:State*) =
		states exists { am(_) }

	def notAny(states:State*) =
		states forall { not(_) }
}
