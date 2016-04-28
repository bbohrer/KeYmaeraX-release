package edu.cmu.cs.ls.keymaerax.btactics.cexsearch

/**
  * Created by hgommers on 4/27/2016.
  */
case class BoundedDFS (maxDepth: Int) extends (SearchNode => Option[ConcreteState]) {
  def dfs (front: List[List[SearchNode]], maxDepth: Int) : Option[ConcreteState] = {
    var frontier = front
    var visited = Set.empty[SearchNode]
    while (true) {
      frontier match {
        case Nil => return None
        case Nil :: rest => frontier = rest
        case (currNode :: siblings) :: ancestors =>
          if (visited.contains(currNode)) {
            frontier = siblings :: ancestors
          } else {
            visited = visited + currNode
            (currNode.goal, front.length >= maxDepth) match {
              case (Some(g), _) => return Some(g)
              case (None, true) => frontier = siblings :: ancestors
              case (None, false) =>
                val littles = currNode.children.toList
                littles match {
                  case Nil =>
                    frontier = siblings :: ancestors
                  case _ =>
                    frontier = littles :: frontier
                }
            }
          }
      }
    }
    None
  }

  def apply(node:SearchNode) = dfs(List(List(node)), maxDepth)
}
