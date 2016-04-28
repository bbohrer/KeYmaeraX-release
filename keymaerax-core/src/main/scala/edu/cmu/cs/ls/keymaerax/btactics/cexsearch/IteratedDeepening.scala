package edu.cmu.cs.ls.keymaerax.btactics.cexsearch

/**
  * Created by hgommers on 4/27/2016.
  */
class IteratedDeepening extends (SearchNode => Option[ConcreteState]) {

    def apply(node:SearchNode) = {

      var currDepth = 0
      while (true) {
        val found = BoundedDFS(currDepth + 1)

        found match {
          case None => currDepth = currDepth + 1
          case Some(g) => return Some(g)
        }
      }
    }
}

