package simon
package state

/** Shows the title screen and waits for Start to be pressed. */
class Title extends StartWaitState
{
	def onStart()
	{
		main pushState new Main()
	}
}