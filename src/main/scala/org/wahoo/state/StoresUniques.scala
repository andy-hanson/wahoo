package org.wahoo
package state

import
	gameObject.Unique,
	reflect.ClassCatalog

import
	scala.collection.mutable,
	scala.reflect.{classTag, ClassTag}

/** For when a Unique is looked for but can't be found.
  * This usually means the writer of the level forgot to add that Unique object.
  */
class UniqueMissingException(klass:Class[_])
	extends RuntimeException
{
	override def toString =
		s"Failed to retrieve unique of class $klass"
}

trait StoresUniques
{
	private var uniques =
		mutable.Map[Class[_], Unique]()

	def addToUniques(obj:Any)
	{
		obj match {
			case u:Unique =>
				addUnique(u)
			case _ =>
				// do nothing
		}
	}

	/** Add a new Unique object.
		* Throws an error if there is already one.
		*/
	def addUnique[A <: Unique](obj:A)
	{
		val klass = uniqueClass(obj)
		require(!uniques.contains(klass), s"Can't add more than one of $klass")
		uniques(klass) = obj
	}

	def uniqueClass(u:Unique) =
		ClassCatalog.superThatImplements(u.getClass, classOf[Unique])

	def isUniqueImplementer(klass:Class[_]) =
		ClassCatalog.directlyImplements(klass, classOf[Unique])

	/** Removes an object from Uniques if it is unique. */
	def removeFromUniques(obj:Any)
	{
		obj match {
			case u:Unique =>
				 uniques -= uniqueClass(u)
			case _ =>
				// do nothing
		}
	}

	/** Retrieves the Unique object of this type.
		* If it has not been added, throws a UniqueMissingException.
		* User must ask for the class that directly implements Unique.
		*/
	def the[A <: Unique : ClassTag]: A =
	{
		val klass = classTag[A].runtimeClass
		require(isUniqueImplementer(klass))
		try {
			uniques(klass).asInstanceOf[A]
		}
		catch {
			case e:NoSuchElementException =>
				throw new UniqueMissingException(klass)
		}
	}

	/** Get the Unique object of a type or None if it doesn't exist. */
	def maybeThe[A <: Unique : ClassTag]: Option[A] =
	{
		val klass = classTag[A].runtimeClass
		require(isUniqueImplementer(klass))
		(uniques get klass).asInstanceOf[Option[A]]
	}
}
