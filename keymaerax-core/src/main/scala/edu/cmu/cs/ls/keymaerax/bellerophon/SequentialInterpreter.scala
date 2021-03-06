/**
  * Copyright (c) Carnegie Mellon University.
  * See LICENSE.txt for the conditions of this license.
  */
package edu.cmu.cs.ls.keymaerax.bellerophon

import edu.cmu.cs.ls.keymaerax.bellerophon.RenUSubst
import edu.cmu.cs.ls.keymaerax.btactics.Augmentors
import edu.cmu.cs.ls.keymaerax.core._
import edu.cmu.cs.ls.keymaerax.btactics.Idioms.?

/**
 * Sequential interpreter for Bellerophon tactic expressions.
  *
  * @param listeners Pre- and pos-processing hooks for step-wise tactic execution.
 * @author Nathan Fulton
 * @author Andre Platzer
 */
case class SequentialInterpreter(listeners : Seq[IOListener] = Seq()) extends Interpreter {
  override def apply(expr: BelleExpr, v: BelleValue): BelleValue = {
    if (Thread.currentThread().isInterrupted) {
      //@note end executing the interpreter when its thread gets interrupted
      //@todo throw an error that is easier to identify (for now: irrelevant, since Hydra Future already gone when we throw here)
      throw new BelleError("Execution Stopped")
    }
    listeners.foreach(_.begin(v, expr))
    try {
      val result: BelleValue =
      expr match {
        case named: NamedTactic => apply(named.tactic, v)
        case builtIn: BuiltInTactic => v match {
          case BelleProvable(pr, _) => try {
            BelleProvable(builtIn.execute(pr))
          } catch {
            case e: BelleError => throw e.inContext(BelleDot, pr.prettyString)
          }
          case _ => throw new BelleError(s"Attempted to apply a built-in tactic to a non-Provable value: ${v.getClass.getName}").inContext(BelleDot, "")
        }
        case BuiltInPositionTactic(_) | BuiltInLeftTactic(_) | BuiltInRightTactic(_) | BuiltInTwoPositionTactic(_) | DependentPositionTactic(_) =>
          throw new BelleError(s"Need to apply position tactic ($expr) at a position before evaluating it with top-level interpreter.").inContext(expr, "")
        case AppliedPositionTactic(positionTactic, pos) => v match {
          case BelleProvable(pr, _) => try {
            BelleProvable(positionTactic.apply(pos).computeResult(pr))
          } catch {
            case e: BelleError => throw e.inContext(positionTactic + " at " + pos, pr.prettyString)
          }
        }
        case positionTactic@AppliedTwoPositionTactic(_, posOne, posTwo) => v match {
          case BelleProvable(pr, _) => try {
            BelleProvable(positionTactic.computeResult(pr))
          } catch {
            case e: BelleError => throw e.inContext(positionTactic + " at " + posOne + ", " + posTwo, pr.prettyString)
          }
        }
        case SeqTactic(left, right, location) => {
          val leftResult = try {
            apply(left, v)
          } catch {
            case e: BelleError => throw e.inContext(SeqTactic(e.context, right, location), "Failed left-hand side of &: " + left)
          }

          try {
            apply(right, leftResult)
          } catch {
            case e: BelleError => throw e.inContext(SeqTactic(left, e.context, location), "Failed right-hand side of &: " + right)
          }
        }
        case d: DependentTactic => try {
          val valueDependentTactic = d.computeExpr(v)
          apply(valueDependentTactic, v)
        } catch {
          case e: BelleError => throw e.inContext(d, v.prettyString)
          case e: Throwable => throw new BelleError("Unable to create dependent tactic", e).inContext(d, "")
        }
        case it: InputTactic[_] => try {
          apply(it.computeExpr(), v)
        } catch {
          case e: BelleError => throw e.inContext(it, v.prettyString)
          case e: Throwable => throw new BelleError("Unable to create input tactic", e).inContext(it, "")
        }
        case PartialTactic(child, _) => try {
          apply(child, v)
        } catch {
          case e: BelleError => throw e.inContext(PartialTactic(e.context), "Failed partial child: " + child)
        }
        case EitherTactic(left, right, location) => try {
          val leftResult = apply(left, v)
          (leftResult, left) match {
            case (BelleProvable(p, _), _) if p.isProved => leftResult
            case (_, x: PartialTactic) => leftResult
            case _ => throw new BelleError("Non-partials must close proof.").inContext(EitherTactic(BelleDot, right, location), "Failed left-hand side of |:" + left)
          }
        } catch {
          //@todo catch a little less. Just catching proper tactic exceptions, maybe some ProverExceptions et al., not swallow everything
          case eleft: BelleError =>
            val rightResult = try {
              apply(right, v)
            } catch {
              case e: BelleError => throw e.inContext(EitherTactic(eleft.context, e.context, location), "Failed: both left-hand side and right-hand side " + expr)
            }
            (rightResult, right) match {
              case (BelleProvable(p, _), _) if p.isProved => rightResult
              case (_, x: PartialTactic) => rightResult
              case _ => throw new BelleError("Non-partials must close proof.").inContext(EitherTactic(left, BelleDot, location), "Failed right-hand side of |: " + right)
            }
        }
        case SaturateTactic(child, annotation, location) =>
          var prev: BelleValue = null
          var result: BelleValue = v
          do {
            prev = result
            //@todo effect on listeners etc.
            try {
              result = apply(child, result)
            } catch {
              case e: BelleError => /*@note child no longer applicable */ result = prev
            }
          } while (result != prev)
          result
        case RepeatTactic(child, times, annotation, location) =>
          var result = v
          for (i <- 1 to times) try {
            result = apply(child, result)
          } catch {
            case e: BelleError => throw e.inContext(RepeatTactic(e.context, times, annotation, location),
              "Failed on repetition " + i + " of " + times + ": " + child)
          }
          result
        case BranchTactic(children, location) => v match {
          case BelleProvable(p, _) =>
            if (children.length != p.subgoals.length)
              throw new BelleError("<(e)(v) is only defined when len(e) = len(v), but " + children.length + "!=" + p.subgoals.length).inContext(expr, "")
            //Compute the results of piecewise applications of children to provable subgoals.
            val results: Seq[Provable] =
              (children zip p.subgoals) map (pair => {
                val e_i = pair._1
                val s_i = pair._2
                val ithResult = try {
                  apply(e_i, bval(s_i))
                } catch {
                  case e: BelleError => throw e.inContext(BranchTactic(children.map(c => if (c != e_i) c else e.context), location), "Failed on branch " + e_i)
                }
                ithResult match {
                  case BelleProvable(resultingProvable, _) =>
                    if (resultingProvable.isProved || e_i.isInstanceOf[PartialTactic]) resultingProvable
                    else throw new BelleError("Every branching tactic must close its associated goal or else be annotated as a PartialTactic").inContext(expr, "")
                  case _ => throw new BelleError("Each piecewise application in a Branching tactic should result in a provable.").inContext(expr, "")
                }
              })

            // Compute a single provable that contains the combined effect of all the piecewise computations.
            // The Int is threaded through to keep track of indexes changing, which can occur when a subgoal
            // is replaced with 0 new subgoals.
            val combinedEffect =
              results.foldLeft((p, 0))((op: (Provable, Int), subderivation: Provable) => {
                replaceConclusion(op._1, op._2, subderivation)
              })
            BelleProvable(combinedEffect._1)
          case _ => throw new BelleError("Cannot perform branching on a goal that is not a BelleValue of type Provable.").inContext(expr, "")
        }
        case OnAll(e, location) =>
          val provable = v match {
            case BelleProvable(p, _) => p
            case _ => throw new BelleError("Cannot attempt OnAll with a non-Provable value.").inContext(expr, "")
          }
          //@todo actually it would be nice to throw without wrapping inside an extra BranchTactic context
          try {
            apply(BranchTactic(Seq.tabulate(provable.subgoals.length)(_ => e), location), v)
          } catch {
            case e: BelleError => throw e.inContext(OnAll(e.context, location), "")
          }

        case ChooseSome(options, e, location) =>
          //@todo specialization to A=Formula should be undone
          val opts = options().asInstanceOf[Iterator[Formula]]
          var errors = ""
          while (opts.hasNext) {
            val o = opts.next()
            if (BelleExpr.DEBUG) println("ChooseSome: try " + o)
            val someResult: Option[BelleValue] = try {
              Some(apply(e.asInstanceOf[Formula=>BelleExpr](o.asInstanceOf[Formula]), v))
            } catch { case err: BelleError => errors += "in " + o + " " + err + "\n"; None }
            if (BelleExpr.DEBUG) println("ChooseSome: try " + o + " got " + someResult)
            (someResult, e) match {
              case (Some(BelleProvable(p, _)), _) if p.isProved => return someResult.get
              case (Some(_), x: PartialTactic) => return someResult.get
              case (Some(_), _) => errors += "option " + o + " " + new BelleError("Non-partials must close proof.").inContext(ChooseSome(options, e, location), "Failed option in ChooseSome: " + o) + "\n" // throw new BelleError("Non-partials must close proof.").inContext(ChooseSome(options, e, location), "Failed option in ChooseSome: " + o)
              case (None, _) => // option o had an error, so consider next option
            }
          }
          throw new BelleError("ChooseSome did not succeed with any of its options").inContext(ChooseSome(options, e, location), "Failed all options in ChooseSome: " + options() + "\n" + errors)

        case Let(abbr, value, inner, location) =>
          val (provable,lbl) = v match {
            case BelleProvable(p, l) => (p,l)
            case _ => throw new BelleError("Cannot attempt Let with a non-Provable value.").inContext(expr, "")
          }
          if (provable.subgoals.length != 1)
            throw new BelleError("Let of multiple goals is not currently supported.").inContext(expr, "")

          import Augmentors.SequentAugmentor
          //@todo sometimes may want to offer some unification for: let j(x)=x^2>0 in tactic for sequent mentioning both x^2>0 and (x+y)^2>0 so j(x) and j(x+y).
          val us: USubst = USubst(SubstitutionPair(abbr, value) :: Nil)
          val in: Provable = Provable.startProof(provable.subgoals.head.replaceAll(value, abbr))
          println("INFO: " + expr + " considers\n" + in + "\nfor outer\n" + provable)
          //assert(us(in.conclusion) == provable.subgoals.head, "backsubstitution will ultimately succeed from\n" + in + "\nvia " + us + " to outer\n" + provable)
          apply(inner, new BelleProvable(in)) match {
            case BelleProvable(derivation, _) =>
              val backsubst: Provable = derivation(us)
              BelleProvable(provable(backsubst,0), lbl)
            case e: BelleError => throw e.inContext(expr, "Let expected proved sub-derivation")
          }
        case t@USubstPatternTactic(children, location) => {
          val provable = v match {
            case BelleProvable(p, _) => p
            case _ => throw new BelleError("Cannot attempt US unification with a non-Provable value.").inContext(expr, "")
          }

          if (provable.subgoals.length != 1)
            throw new BelleError("Unification of multi-sequent patterns is not currently supported.").inContext(expr, "")

          //@todo loop through all using the first one whose unificatoin and tactic application ends up being successful as opposed to committing to first unifiable case.
          //Attempt to find a child that unifies with the input.
          val unification: (UnificationMatch.Subst, RenUSubst => BelleExpr) = children.map(pair => {
            val ty = pair._1
            val expr = pair._2
            ty match {
              case SequentType(s) => try {
                Some((UnificationMatch(s, provable.subgoals.head), expr))
              } catch {
                // in contrast to .unifiable, this suppresses "Sequent un-unifiable Un-Unifiable" message, which clutter STDIO.
                case e: UnificationException => None
              }
              case _ => throw new BelleError("Cannot unify non-sequent types.").inContext(t, "")
            }
          })
            .filter(_.isDefined).map(_.get)
            .headOption.getOrElse(throw new BelleError("USubst Pattern Incomplete -- could not find a unifier for any option").inContext(t, ""))

          apply(unification._2(unification._1.asInstanceOf[RenUSubst]), v)
        }
      }
      listeners.foreach(l => l.end(v, expr, Left(result)))
      result
    } catch {
      case err:BelleError =>
        listeners.foreach(l => l.end(v, expr, Right(err)))
        throw err
      case e:Throwable => println("Unknown throwable"); throw e
    }
  }

  /** Maps sequents to BelleProvables. */
  private def bval(s: Sequent) = BelleProvable(Provable.startProof(s))

  /**
   * Replaces the nth subgoal of original with the remaining subgoals of result.
    *
    * @param original A Provable whose nth subgoal is equal to "result".
   * @param n The numerical index of the subgoal of original to rewrite (Seqs are zero-indexed)
   * @param subderivation
   * @return A pair of:
   *         * A new provable that is identical to original, except that the nth subgoal is replaced with the remaining subgoals of result; and
   *         * The new index of the (n+1)th goal. //@todo clarify
   * @todo result is undefined. Subderivation rather
   */
  private def replaceConclusion(original: Provable, n: Int, subderivation: Provable): (Provable, Int) = {
    assert(original.subgoals.length > n, s"$n is a bad index for Provable with ${original.subgoals.length} subgoals: $original")
    if(original.subgoals(n) != subderivation.conclusion)
      throw new BelleError(s"Subgoal #$n of the original provable (${original.subgoals(n)}}) should be equal to the conclusion of the subderivation (${subderivation.conclusion}})")
    val newProvable = original(subderivation, n)
    val nextIdx = if(subderivation.isProved) n else n + 1
    (newProvable, nextIdx)
  }
}