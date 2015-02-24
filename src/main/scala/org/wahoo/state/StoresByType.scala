package org.wahoo
package state

import reflect.{ClassCatalog, ClassMap}
import scala.reflect.{classTag, ClassTag}

/** Keeps a ClassMap so it's easy to reference all of the objects of some class.
	* Inheritor should make sure to call addToByClass and removeFromByClass.
	* This trait is used by GameState and by AABB nodes.
	*/
trait StoresByType
{
	/** Type-safe map of classes to objects of that class. */
	private var classMap =
		new ClassMap()

	/** Catalogues a new object in classMap.
		* If the object is not in A, does nothing.
		*/
	def addToByType(obj:Any)
	{
		classMap.add(obj)
	}

	/** Removes an object from the class catalog. */
	def removeFromByType(obj:Any)
	{
		classMap.remove(obj)
	}

	/** A sequence of all objects of a type. */
	def all[A : ClassTag]: Traversable[A] =
	{
		require(ClassCatalog.isCatalogued(classTag[A].runtimeClass),
			s"${classTag[A].runtimeClass} is not Catalogued.")
		classMap.all[A]
	}

	/** Iterate over all objects of a type. */
	def each[A : ClassTag](f:A => Unit)
	{
		all[A] foreach f
	}

	/** Iterate over all objects of a type that satisfy some predicate. */
	def eachWhen[A : ClassTag](filter:A => Boolean)(f:A => Unit)
	{
		for (obj <- all[A] if filter(obj))
			f(obj)
	}
}
