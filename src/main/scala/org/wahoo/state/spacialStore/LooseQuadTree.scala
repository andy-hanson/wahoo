package org.wahoo
package state
package spacialStore

import
	collection.{ArrayBag, SpaceMatrix},
	gameObject.{GameObject, InArena},
	math.Scalar,
	shape.{Rect, Shape, Vec},
	util.{assertSorted, foreachMergedSorted} //@todo: no assertSorted

import
	scala.collection.mutable,
	scala.reflect.ClassTag

private[spacialStore] class LooseQuadTree(
	size:Vec,
	minNodeSize:Scalar,
	maxNodeSize:Scalar)
	extends SpacialStore
{
	require((maxNodeSize / minNodeSize).isPowerOf2)

	//grid positions are the bottom left corner of the inner half of each node.

	private val nLevels =
		maxNodeSize.powersOf2MoreThan(minNodeSize) + 1

	private val grids =
		(for (level <- (0 until nLevels);
			nSize = nodeSize(level))
			yield SpaceMatrix[LooseQuadTreeNode](size, nSize) { v:Vec =>
				new LooseQuadTreeNode(
					Rect(v, Vec(nSize)),
					Rect(v - Vec(nSize.half),
					Vec(nSize.twice)))
			}).toArray

	/*
	for (level <- levels) {
		val nSize = nodeSize(level)
		grids(level) = SpaceMatrix[LooseQuadTreeNode](size, nSize) { v:Vec =>
			new LooseQuadTreeNode(
				Rect(v, Vec(nSize)),
				Rect(v - Vec(nSize.half),
				Vec(nSize.twice)))
		}
	}
	*/

	private def nodeSize(level:Int) = minNodeSize.timesPowerOf2(level)

	/** Returns the node of the smallest size containing o, and closest to o.
		* If o is out of the arena, returns None.
		*/
	private def properNodeFor(o:InArena): Option[LooseQuadTreeNode] =
	{
		val level =
			o.shape.diameter.powersOf2MoreThan(minNodeSize)

		assert(nodeSize(level) >= o.shape.diameter)
		assert(level == 0 || nodeSize(level - 1) < o.shape.diameter)

		if (level >= nLevels) {
			assert(o.shape.diameter > maxNodeSize)
			throw new RuntimeException(
				s"$o with size $o.size is bigger than maximum spacial store node size $maxNodeSize.")
		}

		assert((o.centerX - o.shape.diameter.half) aboutLessThan o.left)
		assert((o.centerX + o.shape.diameter.half) aboutMoreThan o.right)
		assert((o.centerY - o.shape.diameter.half) aboutLessThan o.bottom)
		assert((o.centerY + o.shape.diameter.half) aboutMoreThan o.top)
		assert(grids(level).elemSize == nodeSize(level))

		val res = grids(level) get o.center

		for (node <- res)
			assert(node.outer.contains(o.shape),
				s"${node} does not contain ${o.shape} at ${o.center}")

		res
	}

	def add(o:InArena)
	{
		needToMove -= o
		properNodeFor(o) match {
			case Some(node) =>
				node add o
			case None =>
				outOfArena(o)
		}
	}

	private def outOfArena(o:InArena)
	{
		assert(!containsShape(o.shape))
		o.node = null
		o.onStageExit()
	}

	def remove(o:InArena)
	{
		try {
			o.node.remove(o)
		}
		catch {
			case e:NullPointerException =>
				throw new RuntimeException(
					s"Asked to remove $o, but it has no node.")
		}
	}

	//private def eachLevel(f:Int => Unit)
	//{
	//	(0 until levels) foreach f
	//}
	//private def levels: Iterable[Int] =
	//	0 until levels

	//private def eachGrid(f:SpaceMatrix[LooseQuadTreeNode] => Unit)
	//{
	//	eachLevel { level => f(grids(level)) }
	//}

	//private def allGrids: Iterable[Grid] =
	//	for (
	//		level <- levels;
	//		grid <- grids(level))
	//		yield grid

	//private def eachNode(f:LooseQuadTreeNode => Unit)
	//{
	//	eachGrid { _.foreach f }
	//}

	private def allNodes: Iterable[LooseQuadTreeNode] =
		for (
			grid <- grids;
			node <- grid.allNodes)
			yield node

	/** Iterate through all nodes colliding with the given shape. */
	// @todo: replace
	private def eachNodeColliding(shape:Shape)(f:LooseQuadTreeNode => Unit) {
		for (grid <- grids) {
			//It's loose, so we need to expand shape by a bit.
			val collideArea = shape expanded grid.elemSize
			grid.foreachInShape(collideArea)(f)
		}
	}

	private def nodesColliding(shape:Shape): Iterable[LooseQuadTreeNode] =
		for (
			grid <- grids;
			collideArea = shape expanded grid.elemSize;
			node <- grid nodesInShape collideArea)
			yield node

	def allDrawObjectsColliding(shape:Shape): Traversable[BufferedIterator[InArena]] =
	{
		val estimatedSize =
			(shape.width * shape.height / minNodeSize.square).twice.toInt
		val ar =
			ArrayBag.empty[BufferedIterator[InArena]](estimatedSize)

		//eachNodeColliding(shape) { node =>
		for (node <- nodesColliding(shape)) {
			assertSorted(node.objects.iterator)(
				GameObject.DrawOrdering.asInstanceOf[Ordering[InArena]])
			ar += node.objects.iterator
		}

		ar
	}

	def callAllStepsColliding(s:Shape)
	{
		//eachNodeColliding(s) { _.stepObjects.foreach { _.step() } }
		for (
			node <- nodesColliding(s);
			obj <- node.stepObjects)
			obj.step()
		cleanUpMovers()
	}

	def drawAllColliding(s:Shape)
	{
		foreachMergedSorted(allDrawObjectsColliding(s))(_.draw())(
			GameObject.DrawOrdering.asInstanceOf[Ordering[InArena]])
	}

	def eachCollidingOfType[A <: InArena : ClassTag](s:Shape)(f:A => Unit)
	{
		def doit(n:LooseQuadTreeNode)
		{
			n.each[A] { o => if (o.collides(s)) f(o) }
		}

		if (Settings.SpacialStoreLessCollide)
			allNodes foreach doit
		else
			nodesColliding(s) foreach doit
			//eachNodeColliding(s)(doit)
	}

	def drawNodesColliding(s:Shape)
	{
		???
	}

	/** List of all objects that must change nodes after the end of the frame.
		* (If this happened immediately, an object might step() more than once.
		*/
	private var needToMove = mutable.Map.empty[InArena, LooseQuadTreeNode]

	def mayNeedToMove(o:InArena) {
		properNodeFor(o) match {
			case Some(node) =>
				if (node == o.node) needToMove -= o
				else needToMove += ((o, node))
			case None =>
				outOfArena(o)
				needToMove -= o
			case _ => //no need to move
		}
	}

	private def cleanUpMovers() {
		needToMove.foreach { case (o:InArena, to:LooseQuadTreeNode) =>
			assert(o.node != to, s"Why is $o going to $to? It's there!")
			assert(properNodeFor(o).get == to,
						 s"$o going to $to but should go to ${properNodeFor(o).get}")
			o.node.remove(o)
			to.add(o)
		}
		needToMove.clear()
	}

	def containsShape(s:Shape) = Rect(Vec.Zero, size).contains(s)
}
