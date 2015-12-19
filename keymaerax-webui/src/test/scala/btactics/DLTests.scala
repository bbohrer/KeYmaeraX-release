package btactics

import edu.cmu.cs.ls.keymaerax.btactics.TactixLibrary._
import edu.cmu.cs.ls.keymaerax.core.Sequent
import edu.cmu.cs.ls.keymaerax.parser.StringConverter._

import scala.collection.immutable.IndexedSeq

/**
 * Tests [[edu.cmu.cs.ls.keymaerax.btactics.DLBySubst]]
 */
class DLTests extends TacticTestBase {

  "Box abstraction" should "work on top-level" in {
    val result = proveBy("[x:=2;]x>0".asFormula, abstractionb(1))
    result.subgoals should have size 1
    result.subgoals.head.ante shouldBe empty
    result.subgoals.head.succ should contain only "\\forall x x>0".asFormula
  }

  it should "work in context" in {
    val result = proveBy("x>0 & z=1 -> [z:=y;][x:=2;]x>0".asFormula, abstractionb(1, 1::1::Nil))
    result.subgoals should have size 1
    result.subgoals.head.ante shouldBe empty
    result.subgoals.head.succ should contain only "x>0 & z=1 -> [z:=y;]\\forall x x>0".asFormula
  }

  it should "work with loops" in {
    val result = proveBy("[{x:=2;}*]x>0".asFormula, abstractionb(1))
    result.subgoals should have size 1
    result.subgoals.head.ante shouldBe empty
    result.subgoals.head.succ should contain only "\\forall x x>0".asFormula
  }

  it should "not introduce a quantifier when the variables are not bound" in {
    val result = proveBy("[x:=2;]y>0".asFormula, abstractionb(1))
    result.subgoals should have size 1
    result.subgoals.head.ante shouldBe empty
    result.subgoals.head.succ should contain only "y>0".asFormula
  }

  "assignb" should "[y:=1;]y>0 to 1>0" in {
    val result = proveBy("[y:=1;]y>0".asFormula, assignb(1))
    result.subgoals should have size 1
    result.subgoals.head.ante shouldBe empty
    result.subgoals.head.succ should contain only "1>0".asFormula
  }

  it should "[y:=1;]y>0 to 1>0 in the antecedent" in {
    val result = proveBy(Sequent(Nil, IndexedSeq("[y:=1;]y>0".asFormula), IndexedSeq()), assignb(-1))
    result.subgoals should have size 1
    result.subgoals.head.ante should contain only "1>0".asFormula
    result.subgoals.head.succ shouldBe empty
  }

  it should "[y:=1;][z:=2;](y>0 & z>0)" in {
    val result = proveBy("[y:=1;][z:=2;](y>0 & z>0)".asFormula, assignb(1))
    result.subgoals should have size 1
    result.subgoals.head.ante shouldBe empty
    result.subgoals.head.succ should contain only "[z:=2;](1>0 & z>0)".asFormula
  }

  it should "not replace bound variables" in {
    val result = proveBy("[y:=1;][y:=2;]y>0".asFormula, assignb(1))
    result.subgoals should have size 1
    result.subgoals.head.ante shouldBe empty
    result.subgoals.head.succ should contain only "[y:=2;]y>0".asFormula
  }

  it should "only replace free but not bound variables with new universally quantified variable" in {
    val result = proveBy("[y:=1;][y:=2+y;]y>0".asFormula, assignb(1))
    result.subgoals should have size 1
    result.subgoals.head.ante shouldBe empty
    result.subgoals.head.succ should contain only "[y:=2+1;]y>0".asFormula
  }

  it should "replace free variables in ODEs with new universally quantified variable" in {
    val result = proveBy("[y:=1;][{z'=2+y}](y>0 & z>0)".asFormula, assignb(1))
    result.subgoals should have size 1
    result.subgoals.head.ante shouldBe empty
    result.subgoals.head.succ should contain only "[{z'=2+1}](1>0 & z>0)".asFormula
  }

  it should "work when ODE does not write variable, even if it is not top-level" in {
    val result = proveBy("[x:=1;][t:=0;{y'=1}{z:=2;}*]x>0".asFormula, assignb(1))
    result.subgoals should have size 1
    result.subgoals.head.ante shouldBe empty
    result.subgoals.head.succ should contain only "[t:=0;{y'=1}{z:=2;}*]1>0".asFormula
  }

  it should "work when must-bound before ODE, even if it is somewhere in propositional context" in {
    val result = proveBy("[x:=1;](y>2 -> \\forall x [{x'=1}]x>0)".asFormula, assignb(1))
    result.subgoals should have size 1
    result.subgoals.head.ante shouldBe empty
    result.subgoals.head.succ should contain only "y>2 -> \\forall x [{x'=1}]x>0".asFormula
  }

  it should "[y:=y+1;]y>0 to y+1>0" in {
    val result = proveBy("[y:=y+1;]y>0".asFormula, assignb(1))
    result.subgoals should have size 1
    result.subgoals.head.ante shouldBe empty
    result.subgoals.head.succ should contain only "y+1>0".asFormula
  }

  it should "work in front of a loop" in {
    val result = proveBy("[x:=1;][{x:=x+1;}*]x>0".asFormula, assignb(1))
    result.subgoals should have size 1
    result.subgoals.head.ante should contain only "x=1".asFormula
    result.subgoals.head.succ should contain only "[{x:=x+1;}*]x>0".asFormula
  }

  it should "not touch other assignments and formulas when undoing stuttering" in {
    val result = proveBy(Sequent(Nil,
      IndexedSeq("x=2".asFormula, "[x:=2;]x=2".asFormula),
      IndexedSeq("[x:=1;][{x:=x+1;}*]x>0".asFormula, "[x:=3;]x>2".asFormula)), assignb(1))
    result.subgoals should have size 1
    result.subgoals.head.ante should contain only ("x_0=2".asFormula, "[x:=2;]x=2".asFormula, "x=1".asFormula)
    result.subgoals.head.succ should contain only ("[{x:=x+1;}*]x>0".asFormula, "[x:=3;]x>2".asFormula)
  }

  it should "work in front of a loop in the antecedent" in {
    val result = proveBy(Sequent(Nil, IndexedSeq("[x:=1;][{x:=x+1;}*]x>0".asFormula), IndexedSeq()), assignb(-1))
    result.subgoals should have size 1
    result.subgoals.head.ante should contain only "\\forall x (x=1 -> [{x:=x+1;}*]x>0)".asFormula
    result.subgoals.head.succ shouldBe empty
  }

  it should "work in front of a loop in context" in {
    val result = proveBy(Sequent(Nil, IndexedSeq("x=2".asFormula), IndexedSeq("[y:=2;][x:=1;][{x:=x+1;}*]x>0".asFormula)), assignb(1, 1::Nil))
    result.subgoals should have size 1
    result.subgoals.head.ante should contain only "x_0=2".asFormula
    result.subgoals.head.succ should contain only "[y:=2;]\\forall x (x=1 -> [{x:=x+1;}*]x>0)".asFormula
  }

  it should "work in front of a loop in context that binds x" in {
    val result = proveBy("[x:=3;][y:=2;][x:=1;][{x:=x+1;}*]x>0".asFormula, assignb(1, 1::1::Nil))
    result.subgoals should have size 1
    result.subgoals.head.ante shouldBe empty
    result.subgoals.head.succ should contain only "[x:=3;][y:=2;]\\forall x (x=1 -> [{x:=x+1;}*]x>0)".asFormula
  }

  it should "work in front of an ODE, even if it is not top-level" in {
    val result = proveBy("[x:=1;][t:=0;{x'=1}]x>0".asFormula, assignb(1))
    result.subgoals should have size 1
    result.subgoals.head.ante should contain only "x=1".asFormula
    result.subgoals.head.succ should contain only "[t:=0;{x'=1}]x>0".asFormula
  }

  it should "work in front of an ODE system, even if it is not top-level" in {
    val result = proveBy("[x:=1;][t:=0;{x'=1,y'=2}]x>0".asFormula, assignb(1))
    result.subgoals should have size 1
    result.subgoals.head.ante should contain only "x=1".asFormula
    result.subgoals.head.succ should contain only "[t:=0;{x'=1,y'=2}]x>0".asFormula
  }

  it should "work in front of an ODE, even if it is not in the next box" in {
    val result = proveBy("[x:=1;][t:=0;][t:=1;{x'=1}]x>0".asFormula, assignb(1))
    result.subgoals should have size 1
    result.subgoals.head.ante should contain only "x=1".asFormula
    result.subgoals.head.succ should contain only "[t:=0;][t:=1;{x'=1}]x>0".asFormula
  }

  it should "work in front of an ODE, even if it is somewhere in propositional context" in {
    val result = proveBy("[x:=1;](y>2 -> [{x'=1}]x>0)".asFormula, assignb(1))
    result.subgoals should have size 1
    result.subgoals.head.ante should contain only "x=1".asFormula
    result.subgoals.head.succ should contain only "y>2 -> [{x'=1}]x>0".asFormula
  }

  it should "not rename assignment lhs in may-bound" in {
    val result = proveBy("[x:=z;][y:=y_0;{y:=y+1; ++ x:=x-1;}]x<=y".asFormula, assignb(1))
    result.subgoals should have size 1
    result.subgoals.head.ante should contain only "x=z".asFormula
    result.subgoals.head.succ should contain only "[y:=y_0;{y:=y+1; ++ x:=x-1;}]x<=y".asFormula
  }

  it should "not rename must-bound x" in {
    val result = proveBy("[x:=z;][y:=y_0;{x:=x;y:=y+1; ++ x:=x-1;}]x<=y".asFormula, assignb(1))
    result.subgoals should have size 1
    result.subgoals.head.ante shouldBe empty
    result.subgoals.head.succ should contain only "[y:=y_0;{x:=z;y:=y+1; ++ x:=z-1;}]x<=y".asFormula
  }

  "generalize" should "introduce intermediate condition" in {
    val result = proveBy("[x:=2;][y:=x;]y>1".asFormula, generalize("x>1".asFormula)(1))
    result.subgoals should have size 2
    result.subgoals(0).ante shouldBe empty
    result.subgoals(0).succ should contain only "[x:=2;]x>1".asFormula
    result.subgoals(1).ante should contain only "x>1".asFormula
    result.subgoals(1).succ should contain only "[y:=x;]y>1".asFormula
  }

  it should "introduce intermediate condition in context" in {
    val result = proveBy("a=2 -> [z:=3;][x:=2;][y:=x;]y>1".asFormula, generalize("x>1".asFormula)(1, 1::1::Nil))
    result.subgoals should have size 2
    result.subgoals(0).ante shouldBe empty
    result.subgoals(0).succ should contain only "a=2 -> [z:=3;][x:=2;]x>1".asFormula
    result.subgoals(1).ante should contain only "x>1".asFormula
    result.subgoals(1).succ should contain only "[y:=x;]y>1".asFormula
  }

  "postCut" should "introduce implication" in {
    val result = proveBy("[x:=2;][y:=x;]y>1".asFormula, postCut("x>1".asFormula)(1))
    result.subgoals should have size 2
    result.subgoals(0).ante shouldBe empty
    result.subgoals(0).succ should contain only "[x:=2;](x>1 -> [y:=x;]y>1)".asFormula
    result.subgoals(1).ante shouldBe empty
    result.subgoals(1).succ should contain only "[x:=2;]x>1".asFormula
  }

  it should "introduce implication in context" in {
    val result = proveBy("a=2 -> [z:=3;][x:=2;][y:=x;]y>1".asFormula, postCut("x>1".asFormula)(1, 1::1::Nil))
    result.subgoals should have size 2
    result.subgoals(0).ante shouldBe empty
    result.subgoals(0).succ should contain only "a=2 -> [z:=3;][x:=2;](x>1 -> [y:=x;]y>1)".asFormula
    result.subgoals(1).ante shouldBe empty
    result.subgoals(1).succ should contain only "[x:=2;]x>1".asFormula
  }

  "I" should "work on a simple example" in {
    val result = proveBy(Sequent(Nil, IndexedSeq("x>2".asFormula), IndexedSeq("[{x:=x+1;}*]x>0".asFormula)),
      I("x>1".asFormula)(1))

    result.subgoals should have size 3
    // use case
    result.subgoals(0).ante should contain only "x>1".asFormula
    result.subgoals(0).succ should contain only "x>0".asFormula
    // init
    result.subgoals(1).ante should contain only "x>2".asFormula
    result.subgoals(1).succ should contain only "x>1".asFormula
    // step
    result.subgoals(2).ante should contain only "x>1".asFormula
    result.subgoals(2).succ should contain only "[x:=x+1;]x>1".asFormula
  }

  it should "keep constants around" in {
    val result = proveBy(Sequent(Nil,
      IndexedSeq("x>2".asFormula, "y>0".asFormula),
      IndexedSeq("[{x:=x+y;}*]x>0".asFormula)),
      I("x>1".asFormula)(1))

    result.subgoals should have size 3
    // use case
    result.subgoals(0).ante should contain only ("x>1".asFormula, "y>0".asFormula)
    result.subgoals(0).succ should contain only "x>0".asFormula
    // init
    result.subgoals(1).ante should contain only ("x>2".asFormula, "y>0".asFormula)
    result.subgoals(1).succ should contain only "x>1".asFormula
    // step
    result.subgoals(2).ante should contain only ("x>1".asFormula, "y>0".asFormula)
    result.subgoals(2).succ should contain only "[x:=x+y;]x>1".asFormula
  }

  it should "wipe all formulas mentioning bound variables from the context" in {
    val result = proveBy(Sequent(Nil,
      IndexedSeq("x>0".asFormula, "y>1".asFormula, "z>7".asFormula),
      IndexedSeq("[{x:=2;}*]x>2".asFormula, "x<3".asFormula, "y<4".asFormula)), I("x*y>5".asFormula)(1))

    result.subgoals should have size 3
    // use case
    result.subgoals(0).ante should contain only ("x*y>5".asFormula, "y>1".asFormula, "z>7".asFormula)
    result.subgoals(0).succ should contain only "x>2".asFormula
    // init
    result.subgoals(1).ante should contain only ("x>0".asFormula, "y>1".asFormula, "z>7".asFormula)
    result.subgoals(1).succ should contain only ("x<3".asFormula, "y<4".asFormula, "x*y>5".asFormula)
    // step
    result.subgoals(2).ante should contain only ("x*y>5".asFormula, "y>1".asFormula, "z>7".asFormula)
    result.subgoals(2).succ should contain only "[x:=2;]x*y>5".asFormula
  }

  it should "do the same with a slightly more complicated formula" in {
    val result = proveBy(Sequent(Nil,
        IndexedSeq("x>0".asFormula, "y>1".asFormula, "z>7".asFormula),
        IndexedSeq("[{x:=2; ++ y:=z;}*]x>2".asFormula, "x<3".asFormula, "y<4".asFormula)), I("x*y>5".asFormula)(1))

    result.subgoals should have size 3
    // use case
    result.subgoals(0).ante should contain only ("x*y>5".asFormula, "z>7".asFormula)
    result.subgoals(0).succ should contain only "x>2".asFormula
    // init
    result.subgoals(1).ante should contain only ("x>0".asFormula, "y>1".asFormula, "z>7".asFormula)
    result.subgoals(1).succ should contain only ("x<3".asFormula, "y<4".asFormula, "x*y>5".asFormula)
    // step
    result.subgoals(2).ante should contain only ("x*y>5".asFormula, "z>7".asFormula)
    result.subgoals(2).succ should contain only "[x:=2; ++ y:=z;]x*y>5".asFormula
  }

  it should "remove duplicated formulas" in {
    val result = proveBy(Sequent(Nil,
        IndexedSeq("x>0".asFormula, "x>0".asFormula, "y>1".asFormula, "z>7".asFormula),
        IndexedSeq("[{x:=2;}*]x>2".asFormula, "x<3".asFormula, "x<3".asFormula, "y<4".asFormula)), I("x*y>5".asFormula)(1))

    result.subgoals should have size 3
    // use case
    result.subgoals(0).ante should contain only ("x*y>5".asFormula, "y>1".asFormula, "z>7".asFormula)
    result.subgoals(0).succ should contain only "x>2".asFormula
    // init
    result.subgoals(1).ante should contain only ("x>0".asFormula, "y>1".asFormula, "z>7".asFormula)
    result.subgoals(1).succ should contain only ("x<3".asFormula, "y<4".asFormula, "x*y>5".asFormula)
    // step
    result.subgoals(2).ante should contain only ("x*y>5".asFormula, "y>1".asFormula, "z>7".asFormula)
    result.subgoals(2).succ should contain only "[x:=2;]x*y>5".asFormula
  }
}