package edu.cmu.cs.ls.keymaerax.btactics.cexsearch

import scala.collection.mutable

/**
  * Created by bbohrer on 4/27/16.
  */

case class QueueElement(node:SearchNode, dist: Double) extends Ordered[QueueElement] {
  val heuristicValue = node.value
  def compare(that:QueueElement):Int = {
    (heuristicValue + dist).compare(that.heuristicValue + that.dist)
  }
}
object AStar extends ((SearchNode, Long) => Option[ConcreteState]) {

  def astar(frontier:mutable.PriorityQueue[QueueElement], stopBy:Long):Option[ConcreteState] = {
    while (frontier.nonEmpty) {
      if (System.currentTimeMillis() > stopBy) {
        return None
      }
      frontier.dequeue match {
        case QueueElement (node, dist) =>
          node.goal match {
            case Some(goal) => return Some(goal)
            case None =>
              val kids = node.children.map { case child => QueueElement(child, dist + 1) }
              kids.foreach((kid) => frontier.enqueue(kid))
          }
      }
    }
    None
  }

  def apply(node:SearchNode, stopBy:Long) = astar(mutable.PriorityQueue(QueueElement(node, 0)), stopBy)
}
