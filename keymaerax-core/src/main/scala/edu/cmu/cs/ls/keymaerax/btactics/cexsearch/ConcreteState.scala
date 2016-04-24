package edu.cmu.cs.ls.keymaerax.btactics.cexsearch
import edu.cmu.cs.ls.keymaerax.core.NamedSymbol

/**
  * Created by bbohrer on 4/24/16.
  */
class ConcreteState (map: Map[NamedSymbol, Number])extends (NamedSymbol => Number) {
  def apply(ns:NamedSymbol) = map(ns)
}
