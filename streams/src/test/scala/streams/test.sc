import streams.Bloxorz.Level1
import streams.{GameDef, Solver, StringParserTerrain}

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


val s = Level1.Pos(1,1)
val b = Level1.Block(s,s)

b.neighbors.size
b.legalNeighbors.size

Level1.neighborsWithHistory(b, List()).take(5).toList.size



Level1.pathsFromStart.take(5).toList.map(x=>x._1)


