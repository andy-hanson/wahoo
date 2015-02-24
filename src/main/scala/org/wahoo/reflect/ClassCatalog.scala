package org.wahoo
package reflect

import gameObject.Catalogued,
			 util.memoize

object ClassCatalog {
	/** Whether a class is catalogued.
	  * An object of this class will still be catalogued in all catalogued superclasses.
	  * Classes marked DoNotCatalog (but not necessarily their subclasses) are left out.
	  * Any class that is Unique or a subclass of Unique is not catalogued.
	  * Object and ScalaObject are not catalogued.
	  */
	val isCatalogued =
		memoize[Class[_], Boolean] {
			_.getAnnotation(classOf[Catalogued]) != null
		}

	/** Whether a class supports a trait.
		* True if it inherits from a class that implements the trait.
		*/
	def implements(klass:Class[_], trate:Class[_]) =
		trate isAssignableFrom klass

	/** Whether a class implements a trait directly.
	  * Not necessarily true if it inherits from a class that implements the trait.
	  * Multiple classes can separately implement the trait.
	  */
	val directlyImplements =
		memoize[Class[_], Class[_], Boolean] { (klass, trate) =>
			klass.getInterfaces.contains(trate)
		}

	/** Given that this class implements the trait,
	  *   what does it inherit from that directly implements it?
	  * Returns the _first_ class or trait going up the heirarchy
	  *   that directly implements the given trait.
	  */
	val superThatImplements: (Class[_], Class[_]) => Class[_] =
		memoize[Class[_], Class[_], Class[_]] { (klass, trate) =>
			require(implements(klass, trate))
			if (directlyImplements(klass, trate))
				klass
			else
				klass.getInterfaces find {
					directlyImplements(_, trate)
				} getOrElse superThatImplements(klass.getSuperclass, trate)
		}

	/** A List of all superclasses and traits of klass, including klass itself.
		* Memoized because the class structure can never change.
		* Does not repeat any trait even if it is declared twice.
		*/
	val allCataloguedSupertypes =
		memoize[Class[_], Array[Class[_]]] {
			klass:Class[_] => setOfCataloguedSupertypes(klass).toArray
		}

	/*private def setOfCataloguedSuperclasses(klass:Class[_]): Set[Class[_]] = {
		if (klass == null) Set.empty
		else {
			val allHere = (klass.getInterfaces :+ klass).filter(isCatalogued(_)).toSet
			allHere ++ setOfCataloguedSuperclasses(klass.getSuperclass)
		}
	}*/
	private def setOfCataloguedSupertypes(klass:Class[_]): Set[Class[_]] =
	{
		if (klass == null)
			Set.empty
		else
		{
			val allAboveThis =
				klass.getInterfaces.toSet + klass.getSuperclass
			val here: Set[Class[_]] =
				if (isCatalogued(klass))
					Set(klass)
				else
					Set.empty

			allAboveThis.foldLeft(here) { (set, k) => set ++ setOfCataloguedSupertypes(k) }
		}
	}

	def eachCataloguedSuperclass(klass:Class[_])(f:Class[_] => Unit)
	{
		allCataloguedSupertypes(klass) foreach f
	}
}
