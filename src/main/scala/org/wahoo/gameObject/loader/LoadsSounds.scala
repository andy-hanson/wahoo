package org.wahoo
package gameObject
package loader

import
	resource.Sound,
	util.packageToDirectoryName

trait LoadsSounds
{
	def SoundDirectoryName =
		getClass.getSimpleName

	def SoundDirectory =
		s"${packageToDirectoryName(getClass)}/$SoundDirectoryName"

	def playSound(name:String)
	{
		Sound(SoundDirectory, name).play()
	}
}

