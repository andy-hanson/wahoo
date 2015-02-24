package org.wahoo
package gameObject
package graphic
package particle

import
	collection.FixedLengthQueue,
	color.Color,
	light.EmitsLight,
	math.implicits._,
	math.Scalar,
	shape.{Rect, Vec}

import scala.reflect.ClassTag

/** Faster particle effects.
	* Points are stored in an array and are all drawn at once.
	* Instance class must define shape as the region particles are expected to exist in.
	*/
abstract class ParticleEffect[ParticleType <: Particle : ClassTag]
	extends HasShape
{
	var particles = FixedLengthQueue(MaxParticles, newParticle())

	def MaxParticles: Int
	def newParticlesPerFrame: Scalar

	def newParticle(): ParticleType

	private var needToAdd: Scalar = 0

	override def step()
	{
		super.step()
		updateParticles()
	}

	def updateParticles()
	{
		for (particle <- particles)
			particle.move()
		maybeAdd()
	}

	def maybeAdd()
	{
		needToAdd += newParticlesPerFrame
		while (needToAdd > 0) {
			needToAdd -= 1
			particles enqueueAndDequeue newParticle()
		}
	}

	override def draw()
	{
		super.draw()
		render.manyVecs {
			for (particle <- particles)
				render.preparedVec(particle.pos, particle.color)
		}
	}
}

abstract class ArenaParticleEffect[ParticleType <: Particle : ClassTag](
	initPos:Vec)
	extends ParticleEffect[ParticleType]
	with InArena
	with HasMutableShape
{
	var pos = initPos

	shape = Rect zeroAt initPos

	override def updateParticles()
	{
		var minX = pos.x//Scalar.MaxValue
		var maxX = pos.x//Scalar.MinValue
		var minY = pos.y//Scalar.MaxValue
		var maxY = pos.y//Scalar.MinValue

		for (p <- particles) {
			p.move()

			minX = p.pos.x min minX
			maxX = p.pos.x max maxX
			minY = p.pos.y min minY
			maxY = p.pos.y max maxY
		}

		shape = Rect fromTo (minX, minY, maxX, maxY)

		maybeAdd()
	}
}

abstract class LitParticleEffect[ParticleType <: Particle : ClassTag](
	pos:Vec)
	extends ArenaParticleEffect[ParticleType](pos)
	with EmitsLight
{
	def lightRelevantArea =
		shape

	def applyLight()
	{
		for (particle <- particles)
			lightTracker.alterColorAt(particle.pos) {
				_ += particle.color
			}
	}
}

abstract class Particle
{
	def color: Color
	def pos: Vec
	def move()
}

abstract class VelParticle(initPos:Vec, initVel:Vec) extends Particle {
	var pos = initPos
	var vel = initVel
	def move()
	{
		pos += vel
	}
}

abstract class ProjectileParticle(initPos:Vec, initVel:Vec)
	extends VelParticle(initPos, initVel)
{
	def gravity: Scalar

	override def move()
	{
		super.move()
		vel = vel.down(gravity)
	}
}

/*
abstract class LitParticle extends Particle {
	def light(v:Vec): Color
}
*/

class LinearParticle(var pos:Vec, vel:Vec, val color:Color) extends Particle
{
	def move()
	{
		pos += vel
	}
}
