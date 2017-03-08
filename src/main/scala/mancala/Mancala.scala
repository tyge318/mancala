package mancala

/**
  * Created by Frankle on 2017/3/1.
  */

object Mancala extends App{
  //begin of sub class and sub function define
  case class Board(status: String = "0 4 4 4 4 4 4 4 0; 0 4 4 4 4 4 4 4 0") {
    var rows = status.split(";")
    val colSize = rows(0).split(" ").length
    var board = Array.ofDim[Int](2, colSize)
    for( i<- 0 to 1)  board(i) = rows(i).trim.split(" ").map(x => x.toInt)

    def getBoardString = board.deep.mkString("\n")
    def move(playerID: Int, pitID: Int): Boolean = {
      var stones = board(playerID)(pitID)
      val numPits = colSize*2-3
      board(playerID)(pitID) = 0
      val offset = stones / numPits
      if( offset > 0) {
        for (i <- 0 to 1) {
          for (j <- 0 to colSize - 1) {
            if( playerID == 0 && j != colSize-1 || playerID == 1 && j != 0)
              board(i)(j) += offset
          }
        }
      }
      stones %= numPits
      var curPit = pitID
      var curRow = playerID
      while( stones > 0) {
        if( curRow == 1) {
          curPit += 1
          if( playerID == 0 && curPit == colSize-1) {
            curRow = 0
            curPit -= 1
          }
          board(curRow)(curPit) += 1
          if(curPit == colSize-1) {
            board(0)(curPit) = board(1)(curPit)
            curRow = 0
          }
        }
        else {
          curPit -= 1
          if( playerID == 1 && curPit == 0 ) {
            curRow = 1
            curPit += 1
          }
          board(curRow)(curPit) += 1
          if( curPit == 0) {
            board(1)(0) = board(0)(0)
            curRow = 1
          }
        }
        stones -= 1
      }
      if( curRow == playerID && board(curRow)(curPit) == 1) {
        val myMancala = if(playerID == 0) 0 else colSize-1
        val opponent = if(playerID == 1) 0 else 1
        board(playerID)(myMancala) += (board(opponent)(curPit) + 1)
        board(opponent)(myMancala) = board(playerID)(myMancala)
        board(opponent)(curPit) = 0
        board(playerID)(curPit) = 0
      }
      ( playerID == 1 && curPit == colSize-1 || playerID == 0 && curPit == 0 )  //whether to do a next move
    }
    def allEmpty(playerID: Int): Boolean = board(playerID).forall( x => x == 0)
    def eval(playerID: Int): Int = if( playerID == 0) (board(playerID)(colSize-1) - board(0)(0)) else (board(0)(0) - board(playerID)(colSize-1))

    override def toString: String = board(0).mkString(" ")+"; "+board(1).mkString(" ")
    def getState = {
      board(0).slice(1, colSize-1).mkString(" ") :: board(1).slice(1, colSize-1).mkString(" ") :: board(0)(0).toString :: board(1)(colSize-1).toString :: Nil
    }
  }
  def runMancala(mode: String, playerID: String, cutOffDepth: String, rowA: String, rowB: String, mancalaA: String, mancalaB: String): List[String] = {
    val pruning = if( mode.equals("3")) true else false
    val rootPlayer = if( playerID.equals("1") ) 0 else 1
    val otherPlayer = if( playerID == 0) 1 else 0
    val depth = cutOffDepth.toInt
    val boardString = mancalaA+" " + rowA + " " + mancalaB+"; "+mancalaA+ " " +rowB+ " " + mancalaB

    val logBuf = new StringBuilder
    var myBoard = Board(boardString)

    case class Node(playerID: Int, maxNode: Boolean, name: String, b: Board, curDepth: Int, af: Int = Integer.MIN_VALUE, bt: Int = Integer.MAX_VALUE){
      var score = if(maxNode) Integer.MIN_VALUE else Integer.MAX_VALUE
      var board = b
      var this.curDepth = curDepth
      var this.name = name
      var this.playerID = playerID
      var alpha = af
      var beta = bt
      var otherPlayer = if( playerID == 0) 1 else 0
      var bestResult: Board = b
      var movePath: String = this.name

      def evalScore: Int = board.eval(rootPlayer)
      def updateScoreTemp(newScore: Int):Unit = {
        if(maxNode && newScore > score || !maxNode && newScore < score) {
          score = newScore
        }
      }
      def updateScore(newScore: Int, pruning: Boolean, nextStatus: Node, updateBoard: Boolean): Int = {
        if(maxNode && newScore > score || !maxNode && newScore < score) {
          score = newScore
          if (updateBoard) {
            bestResult = Board(nextStatus.board.toString)
            movePath = movePath + "=>" + nextStatus.movePath
          }
        }
        score
      }
      def updateAlphaBeta(newScore: Int, pruning: Boolean, isLeaf: Boolean): Unit = {
        if(pruning && !isLeaf) {
          if( maxNode && newScore > alpha)  alpha = newScore
          if( !maxNode && newScore < beta)  beta = newScore
        }
      }
      def logString(pruning: Boolean): String = {
        val scoreStr = convertX(score)
        (this.name + "," + this.curDepth + "," + scoreStr) + (if(pruning) "," + convertX(alpha) + "," + convertX(beta) else "")
      }

      def convertX(x: Int): String = x match {
        case Integer.MIN_VALUE => "-Infinity"
        case Integer.MAX_VALUE => "Infinity"
        case _ => x.toString
      }
    }
    def collectStones(playerID: Int, b: Board): Board = {
      for(i <- 1 until b.colSize-1) {
        if( playerID == 0)  {
          b.board(0)(0) += b.board(0)(i)
          b.board(1)(0) = b.board(0)(0)
        }
        else {
          b.board(1)(b.colSize-1) += b.board(0)(i)
          b.board(0)(b.colSize-1) = b.board(1)(b.colSize-1)
        }
        b.board(0)(i) = 0
      }
      b
    }
    def playMiniMax(status: Node, depth: Int, extra: Boolean, pruning: Boolean): Node = {
      //status is choosing among all possible outcomes of next (other player) round
      val playerID = status.playerID
      val otherPlayer = if(playerID == 0) 1 else 0
      for( i <- 1 until status.b.colSize-1) {
        if( status.b.board(otherPlayer)(i) != 0 && (!pruning || pruning && status.alpha < status.beta) ) {
          val pitName = (if(otherPlayer == 0) "A" else "B") + (i+1)
          //default case: assume no again round
          var curStatus = Node(otherPlayer, !status.maxNode, pitName, Board(status.b.toString), if(!extra) status.curDepth+1 else status.curDepth, status.alpha, status.beta)

          var again = curStatus.board.move(otherPlayer, i)
          val endGame = curStatus.board.allEmpty(otherPlayer)
          if( !endGame) {
            if (again) {
              //again round, current player play again, same node type as current, same depth as current
              curStatus = Node(playerID, status.maxNode, curStatus.name, Board(curStatus.board.toString), curStatus.curDepth, curStatus.alpha, curStatus.beta)
            }
            if (depth != curStatus.curDepth || (depth == curStatus.curDepth && again)) { //print pass down log
              //println(curStatus.logString(pruning))
              logBuf ++= (curStatus.logString(pruning) + "\n")
            }
            if(again || depth != curStatus.curDepth) curStatus = playMiniMax(curStatus, depth, again, pruning)
          }
          else {
            //end game, early termination, otherPlayer can't make any move, playerID collects all stones
            curStatus = curStatus.copy(b = collectStones(playerID, curStatus.board) )
          }

          var score = if(!again && depth == curStatus.curDepth || endGame ) curStatus.evalScore else curStatus.score
          score = curStatus.updateScore(score, pruning, curStatus, true)
          val resolveLog = curStatus.logString(pruning)
          if( (depth == curStatus.curDepth && !again) || endGame) {
            //println(resolveLog)
            logBuf ++= (resolveLog + "\n")
          }
          curStatus.updateAlphaBeta(score, pruning, (!again && depth == curStatus.curDepth || endGame))
          val updateBoard = (status.curDepth <= 1 && curStatus.curDepth <= 1 )
          status.updateScore(score, pruning, curStatus, updateBoard)
          //println(status.logString(pruning))
          logBuf ++= (status.logString(pruning) + "\n")
          status.updateAlphaBeta(score, pruning, false)
        }
      }
      status.board = status.bestResult
      status
    }

    var root = Node(playerID = rootPlayer, maxNode = true, name = "root", myBoard, curDepth = 0)
    var output = playMiniMax(root, depth = (if(mode.equals("1")) 1 else depth), extra = false, pruning = pruning)

    val outputState = output.bestResult.getState
    outputState ::: List(output.movePath, logBuf.toString)
  }
  def makeMove(rowA: String, rowB: String, mancalaA: String, mancalaB: String, moveID: String): List[String] = {
    val boardString = mancalaA+" " + rowA + " " + mancalaB+"; "+mancalaA+ " " +rowB+ " " + mancalaB
    var myBoard = Board(boardString)
    val targetPit = moveID.replace("move", "")
    val playerID = if(targetPit.startsWith("B")) 1 else 0
    val pitID = if(playerID == 1) targetPit.replace("B", "").toInt-1 else targetPit.replace("A", "").toInt-1

    val again = myBoard.move(playerID, pitID)
    val outputState = myBoard.getState
    outputState ::: List(again.toString)
  }
  //val out = runMancala("2", "1", "2", "3 3 3", "3 3 3", "0", "0")
  //out.foreach(println(_))
  val test = makeMove("3 3 3", "3 3 3", "0", "0", "B2")
  test.foreach(println(_))
}
