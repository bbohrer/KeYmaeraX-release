package edu.cmu.cs.ls.keymaerax.btactics.cexsearch

/**
  * Created by hgommers on 4/27/2016.
  */
object IteratedDeepening extends ((SearchNode, Long) => Option[ConcreteState]) {
    def apply(node:SearchNode, stopBy:Long):Option[ConcreteState] = {
      var currDepth = 0
      while (true) {
        if (System.currentTimeMillis() > stopBy) {
          return None
        }
        BoundedDFS(currDepth + 1)(node, stopBy) match {
          case None => currDepth = currDepth + 1
          case Some(g) => return Some(g)
        }
      }
      None
    }
}

