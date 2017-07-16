package streams

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
      * This method applies a list of moves `ls` to the block at position
      * `startPos`. This can be used to verify if a certain list of moves
      * is a valid solution, i.e. leads to the goal.
      */
    def solve(ls: List[Move]): Block =
    ls.foldLeft(startBlock) { case (block, move) => move match {
      case Left => block.left
      case Right => block.right
      case Up => block.up
      case Down => block.down
    }
    }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait LevelX extends SolutionChecker {
    /* terrain for custom level*/

    val level =
      """-----oooooo----
        |-----o--ooo----
        |-----o--ooooo--
        |Sooooo-----oooo
        |----ooo----ooTo
        |----ooo-----ooo
        |------o--oo----
        |------ooooo----
        |------ooooo----
        |-------ooo-----""".stripMargin

  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(1, 1)), "1,1") // start
      assert(terrain(Pos(4, 7)), "4,7") // goal
      assert(terrain(Pos(5, 8)), "5,8")
      assert(!terrain(Pos(5, 9)), "5,9")
      assert(terrain(Pos(4, 9)), "4,9")
      assert(!terrain(Pos(6, 8)), "6,8")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(0, -1)), "0,-1")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
    }
  }

  test("findChar end pos level 1") {
    new Level1 {
      assert(goal == Pos(4, 7))
    }
  }

  test("block in terrain") {
    new Level1 {
      assert(Block(Pos(1, 1), Pos(1, 2)).isLegal, "(1,1)(1,2) is legal")
    }
  }

  test("block out of terrain") {
    new Level1 {
      assert(!Block(Pos(0, 2), Pos(0, 3)).isLegal, "(0,2)(0,3) is out of terrain")
    }
  }

  test("block neighbors") {
    new Level1 {
      println(Block(Pos(1, 1), Pos(1, 2)).neighbors)
      //assert(!Block(Pos(0, 2), Pos(0, 3)).isLegal, "(0,2)(0,3) is out of terrain")
    }
  }

  test("block legal neighbors") {
    new Level1 {
      println(Block(Pos(0, 0), Pos(0, 1)).legalNeighbors)
      //assert(!Block(Pos(0, 2), Pos(0, 3)).isLegal, "(0,2)(0,3) is out of terrain")
    }
  }

  test("neighbors with history") {
    new Level1 {
      val result = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).toSet
      assert(result === Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      ))
    }
  }

  test("new neighbors only") {
    new Level1 {
      val nno = newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
        ).toStream,

        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))
      )
      assert(nno.toSet === Set(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      ))
    }
  }

  test("paths from start") {
    new Level1 {
      println("Printing paths from start")
      println(pathsFromStart.take(10).mkString("\n"))
    }
  }



  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

  test("solve level") {
    new LevelX {
      val result = solution
      println("SOLUTION")
      result.reverse.foreach(println)
    }
  }

}
