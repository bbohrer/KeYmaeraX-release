package btactics.cexsearch

import java.util.TimerTask
import java.util.concurrent.{TimeUnit, TimeoutException}

import edu.cmu.cs.ls.keymaerax.btactics.TacticTestBase
import edu.cmu.cs.ls.keymaerax.bellerophon.BelleError
import edu.cmu.cs.ls.keymaerax.btactics.cexsearch._
import edu.cmu.cs.ls.keymaerax.core._
import edu.cmu.cs.ls.keymaerax.parser.StringConverter._
import edu.cmu.cs.ls.keymaerax.tools.{CounterExampleTool, DiffSolutionTool, ToolBase, ToolEvidence}
import org.joda.time.{DateTime, Duration}

/**
  * Test cases for counterexample search. We generally don't care what the counterexample is as long as it's  a counterexample.
  * Actually evaluating whether a particular example is really a counterexample is nontrivial, so let's just focus on whether
  * it returns a counterexample on false formulas or not.
  * We also care how long different search algos take and how many of our examples they can get right in a given amount of time.
  * Let's make it easy to compare lots of different combinations of search algos and heuristics so we can have some nice graphs
  * for the report.
  * Created by bbohrer on 4/24/16.
  */
class CexSearchTests  extends TacticTestBase {
  val algos: List[((SearchNode, Long) => Option[ConcreteState])] =
    List(
      BreadthFirstSearch,
      BoundedDFS(4),
      BoundedDFS(10),
      BoundedDFS(20),
      IteratedDeepening,
      AStar
      )

  val times: List[Int] = List (
    /*100,
    200,*/
    6400/*,
    800,
    1600,
    3200,
    6400*/
  )
  /* We should strive for a variety of different difficulty levels, and make sure all formulas have the shape P -> [a] Q for
  * program-free P, or else none of this will work.
  * These are "easy" in the sense that no searching of infinite stuff is required and therefore no search heuristics are
  * necessary, let alone good ways. Still hard in the sense of we need to implement symbolic execution correctly before
  * these will work.
  * */
  val easyTrueFmls = List(
    "false -> [?true;] true",
    "x = 2 -> [?true;] x > 1",
    "x = 2 -> [x := 5;] x > 3",
    "x = 2 -> [?false;] x = 3",
    "x = 2 -> [?x < 1;] x = 3",
    "(x = 7 & y < 3) -> [x := x + 1; y := y + x;](x >= 7.3 & y <= 12)",
    "(x = 0 | (x > 0 -> y = 7)) -> [x := x - 1; y := -y;] (x <= -1 | y = -7)",
    "y = x + 2 -> [x := (x + 2)^2 ;++ ?y >= 2;] x >= 0",
    "y' = x' + 2 -> [x' := (x' + 2)^2 ;++ ?y' >= 2;] x' >= 0",
    "true -> [x :=*;] ((x + 2)^8 >= 0)"
  ).map({case str => str.asFormula})

  val easyFalseFmls = List(
    "true -> [?true;] false",
    "x = 2 -> [?true;] x < 1",
    "x = 2 -> [x := 5;] x < 3",
    "x = 2 -> [?true;] x = 3",
    "x = 2 -> [?x > 1;] x = 3",
    "(x = 7 & y < 3) -> [x := x + 1; y := y + x;](x <= 7.3 | y > 12)",
    "(x = 2 | (x > 10 -> y = 7)) -> [x := x - 1; y := -y;] (x <= -1 | y = -7)",
    "y = x + 2 -> [x := (x + 2)^2 ;++ ?y < -10;] x >= 0",
    "y' = x' + 2 -> [x' := (x' + 2)^2 ;++ ?y' < -10;] x' >= 0"
  ).map({case str => str.asFormula})

  val loopFalseFmls = List(
    "true -> [{?true;}*] false",
    "true -> [x :=*;] x > 0",
    "x=0 -> [{x := x + 1;}*] x < 5",
    /* Should fail for DFS, succeed for BFS (after a long time) */
    "true -> [x:=0; {x := x; ++ x := x + 1;}*] x <= 4",
    /* See which branch leads to a contradiction faster... */
    "true -> [x:= 0; {x := x + 1; ++ x := (x+1)^(x + 5);}*] x <= 4",
    /* This will probably break A*, since the "simple" case makes no progress
    * toward the goal, but the "complicated" case does. */
    "true -> [x:= 0; y := 1; {x := x; ++ x := x + y;}*] x <= 4",
    "[x := 0; {{{{x := x + 1;}*}*}*}*] x <= 4",
    "[x := 0; {{{{x := x + 1; ++ y := y + 1 + x; ++ y := 0;}*}*}*}*] x <= 4",
    "[x := 0; {x := x + 1;}*;{x := x + 1;}*] x <= 4",
    "[x := 0; {y := y + 1; {x := x + 1;}*}*] x <= 4",
    "x = 1 & t = 0 & epsilon >= 0 -> [{x := x + x*epsilon; ?x >= 1;}*] x >= 1 + t + (t^2)/2 + (t^3)/6 + (t^4)/24",
    "x = 0 -> [{x := x + 1; ++ x := x + 2; ++ x := x + 4; x := x + 8; ++ x := x + 16; ++ x := x + 32; ++ x := x + 16; ++ x := x + 8; ++ x := x + 4; ++ x := x + 2; ++ x := x + 1;}*] x <= 100"

  ).map({case str => str.asFormula})

  /* Can't hope for a counterexample on most of these */
  val loopTrueFmls = List(
    "x=0 -> [{x := x + 1;}*] x >= 5".asFormula
  )

  val allFalseCases = List.concat(easyFalseFmls,loopFalseFmls)
  val allCases = List.concat(easyTrueFmls, allFalseCases)

  "Every algorithm" should "get the easy true cases right" in withMathematica(implicit qeTool => {
    times.foreach({ case millis =>
      algos.foreach({ case algo =>
        easyTrueFmls.foreach({ case fml =>
          val finishBy = System.currentTimeMillis + millis
          algo(ProgramSearchNode(fml), finishBy).isDefined shouldBe false
        })
      })
    })
  })

  /*  Read more: http://javarevisited.blogspot.com/2015/01/how-to-use-future-and-futuretask-in-Java.html#ixzz47ACDyUbX
  def cancellable[T](f: Future[T])(customCode: => Unit): (() => Unit, Future[T]) = {
    val p = Promise[T]
    val first = Future firstCompletedOf Seq(p.future, f)
    val cancellation: () => Unit = {
      () =>
        first onFailure { case e => customCode}
        p failure new Exception
    }
    (cancellation, first)
  }
*/

  it should "get the false cases right" in withMathematica(implicit qeTool => {
    times.foreach({case millis =>
      var theStr = ""
      algos.foreach({case algo =>
      theStr = theStr + algo.getClass.getSimpleName
      val start = DateTime.now
      allFalseCases.foreach({case fml =>
        val finishBy = System.currentTimeMillis + millis
        try {
          algo(ProgramSearchNode(fml), finishBy) match {
            case Some(result) => theStr = theStr + ",1"
            case None => theStr = theStr + ",0"
          }
        } catch {
          case _ : Throwable => theStr = theStr + ",0"
        }
      })
      val diff = (DateTime.now.millisOfDay.get - start.millisOfDay.get).toDouble / 1000.0
      theStr = theStr + "," + diff + "\n"
    })
    print ("CSV for timeout " + millis + ": \n" + theStr)
  })
  })


  it should "loop on this test" in withMathematica(implicit qeTool => {
    times.foreach({case millis =>
      var theStr = ""
    algos.foreach({ case algo =>
      theStr = theStr + algo.getClass.getSimpleName
      loopTrueFmls.foreach({ case fml =>
        val finishBy = System.currentTimeMillis + millis
        val result = algo(ProgramSearchNode(fml), finishBy)
        theStr = theStr + (if (result.isDefined) { ",1"} else {",0"})
        print("Testing algo " + algo.getClass.getSimpleName + " for falseness of " + fml + "\n")
//        result.isDefined shouldBe true
      })
      theStr = theStr + "\n"
    })
    print ("CSV to the rescue: \n" + theStr)
  })})

  /* HOW TO TIMEOUT
  *

  * */
}
