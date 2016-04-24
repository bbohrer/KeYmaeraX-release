package edu.cmu.cs.ls.keymaerax.btactics.cexsearch

import edu.cmu.cs.ls.keymaerax.core._

/**
  * Given two propositions pre,post and a hybrid program prog, search for a counterexample to pre -> [prog]post.
  * We maintain the invariant that pre is program-free, but post is allowed to have the form [prog'] Q'.
  * Created by bbohrer on 4/24/16.
  */
object ProgramSearchNode {
  def apply(fml:Formula):ProgramSearchNode = {
    fml match {
      case (Imply(pre, Box(prog, post))) =>
        new ProgramSearchNode(pre,prog,post)
      case _ => throw new IllegalArgumentException("ProgramSearchNode expects formula of shape P -> [a] Q")
    }
  }
}
class ProgramSearchNode (pre: Formula, prog: Program, post: Formula) extends AnyRef with SearchNode {

  /* We are at a goal state if there is a counterexample to pre -> [prog] post that we can find without any more
  * search, which is to say there are no programs left. Since our representation requires that we always have some "program",
  * we represent "no program" as the no-op program ?True. Note that the postcondition is allowed to contain programs,
  * so we are not done unless the postcondition is also free of programs.
  *
  * If we are at a goal state, this returns the actual counterexample that we found, otherwise it returns None to indicate
  * absence of a counterexample
  * */
  def goal = (prog, post) match {
    case (_, Box(_,_)) => None
    case (Test(True), _) => None
    case _ => None
  }

  /* Returns a sequence of search states reachable by running this program for one step. The search need not be complete
  * but should be sound, meaning that a counterexample for any child formula constitutes a counterexample for the parent. */
  def children = Set.empty

  /* Heuristic value for this search state. Should be admissible and all that good stuff. Let's pick a heuristic that
  * looks at both how expensive a state is to evaluate and how likely it is to be a counterexample. In particular, since
  * deciding first-order logic formulas is O(2^(2^n)) in number of variables, we should work that into our cost for
  * leaves. */
  def value:Float = 0

}
