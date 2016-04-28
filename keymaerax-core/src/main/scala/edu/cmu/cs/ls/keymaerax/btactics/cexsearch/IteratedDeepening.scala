package edu.cmu.cs.ls.keymaerax.btactics.cexsearch

/**
  * Created by hgommers on 4/27/2016.
  */
case class IteratedDeepening extends (SearchNode => Option[ConcreteState]) {

    def apply(node:SearchNode) = {
      var currDepth = 0
      while (true) {
        BoundedDFS(currDepth + 1)(node) match {
          case None => currDepth = currDepth + 1
          case Some(g) => return Some(g)
        }
      }
    }
}

