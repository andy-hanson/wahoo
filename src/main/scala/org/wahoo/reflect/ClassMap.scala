package org.wahoo
package reflect

import collection.Bag
import scala.collection.mutable,
			 scala.reflect.{classTag, ClassTag}

/** Class to represent when a ClassMap was asked to remove an object it wasn't storing. */
case class NoObjectToRemoveException[A](obj:A)
	extends Exception
{
	override def toString =
		"Can not remove obj "+obj+": it is not in the ClassMap."
}

/** Type-safe map of classes to objects of that class.
  * Catalogues every superclass up to highestSuperclassToStore.
  * Objects put in the map will be stored in every applicable class.
  */
final class ClassMap()
{
	/** Underlying map from class to objects.
	  * Casting is necessary to make this type-safe.
	  */
	private var map =
		mutable.Map[Class[_], Bag[_]]()

	/** Whether the map currently holds a list of objects of this class. */
	def isClassStored(klass:Class[_]) =
		map contains klass

	/** Get all objects of a class.
		* If the class hasn't been catalogued, that means an empty list.
		*/
	def all[A : ClassTag]: Traversable[A] =
		(map get classTag[A].runtimeClass) match {
			case Some(seq) =>
				seq.asInstanceOf[Bag[A]]
			case None =>
				None
		}

	/** Removes a single class from the map (no questions asked) */
	private def removeSingleClass[A](klass:Class[A])
	{
		map -= klass
	}

	/** Puts an object into the map.
	  * This means catalog every superclass and trait that's catalog-able.
	  * If any classes are not already in the map, they are added.
	  */
	def add[A >: Null](obj:A)
	{
		ClassCatalog.eachCataloguedSuperclass(obj.getClass) { sKlass =>
			addIndividual(obj, sKlass.asInstanceOf[Class[_ >: A]])
		}
	}

	/** Add a single object into a single class.
		* Adds the class if it is not yet catalogued.
		*/
	private def addIndividual[A <: B, B >: Null : ClassTag](obj:A, klass:Class[B])
	{
		try {
			map(klass).asInstanceOf[Bag[B]] += obj.asInstanceOf[B]
		}
		catch {
			//If we didn't have a record for this class, add one.
			case e:NoSuchElementException =>
				map += ((klass, Bag.single(obj.asInstanceOf[B])))
		}
	}


	/** Remove an object from the map.
	  * If any classes now have no elements, they are removed from the map.
	  */
	def remove[A](obj:A)
	{
		try {
			ClassCatalog.eachCataloguedSuperclass(obj.getClass) { sKlass =>
				removeIndividual(obj, sKlass.asInstanceOf[Class[_ >: A]])
			}
		}
		catch {
			case e:NoSuchElementException => throw NoObjectToRemoveException(obj)
		}
	}

	/** Remove a single object from a single class.
		* Unlike addIndividual, this should only throw an error
		* if the caller tries to remove a non-existant object.
		* Removes the class if this was the last object in that class.
		*/
	private def removeIndividual[A <: B, B](obj:A, klass:Class[B])
	{
		val objs = map(klass).asInstanceOf[Bag[B]]
		objs -= obj.asInstanceOf[B]
		//Don't bother to store any class there are no objects of.
		if (objs.isEmpty)
			removeSingleClass(klass)
	}
}
