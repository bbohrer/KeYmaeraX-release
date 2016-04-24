package edu.cmu.cs.ls.keymaerax.btactics.cexsearch
import edu.cmu.cs.ls.keymaerax.core.{NamedSymbol, Term}

/**
  * Created by bbohrer on 4/24/16.
  */

case class ConcreteState (map: Map[NamedSymbol, Term])extends (NamedSymbol => Term) {
  def apply(ns:NamedSymbol) = map(ns)
}
