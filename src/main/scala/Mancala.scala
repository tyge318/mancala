/**
  * Created by Frankle on 2017/3/1.
  */
object Mancala extends App{
  case class Board(status: String = "0 4 4 4 4 4 4 4 0; 0 4 4 4 4 4 4 4 0") {
    var rows = status.split(";")
    val colSize = rows(0).split(" ").length
    var board = Array.ofDim[Int](2, colSize)
    for( i<- 0 to 1)  board(i) = rows(i).trim.split(" ").map(x => x.toInt)

    def printBoard = println(board.deep.mkString("\n"))
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
  }
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
/*
    def toggleNodeType: Unit = {
      maxNode = !mn
      score = if(maxNode) Integer.MIN_VALUE else Integer.MAX_VALUE
    } */
    def evalScore: Int = board.eval(rootPlayer)
    def updateScoreTemp(newScore: Int):Unit = {
      if(maxNode && newScore > score || !maxNode && newScore < score) {
        score = newScore
      }
    }
    def updateScore(newScore: Int, pruning: Boolean, newBoard: Board, updateBoard: Boolean): Int = {
      if(maxNode && newScore > score || !maxNode && newScore < score) {
        score = newScore
        if (updateBoard)
          bestResult = Board(newBoard.toString)
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
  def playGreedy(playerID: Int, b: Board): Board = {
    var maxScore = Integer.MIN_VALUE
    var maxBoard = None: Option[Board]
    val otherPlayer = if (playerID == 0) 1 else 0
    for( i <- 1 until b.colSize-1) {
      if( b.board(playerID)(i) != 0) {
        var curBoard = b.copy(b.toString)
        var again = curBoard.move(playerID, i)
        val endGame = curBoard.allEmpty(playerID)
        again = again && !endGame
        curBoard = if(again) playGreedy(playerID, curBoard) else curBoard
        if(endGame) curBoard = collectStones(otherPlayer, curBoard)
        val score = curBoard.eval(playerID)
        if( score > maxScore) {
          maxScore = score
          maxBoard = Some(curBoard)
        }
      }
    }
    maxBoard.get
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
            println(curStatus.logString(pruning))
          }
          if(again || depth != curStatus.curDepth) curStatus = playMiniMax(curStatus, depth, again, pruning)
        }
        else {
          //end game, early termination, otherPlayer can't make any move, playerID collects all stones
          curStatus = curStatus.copy(b = collectStones(playerID, curStatus.board) )
        }

        var score = if(!again && depth == curStatus.curDepth || endGame ) curStatus.evalScore else curStatus.score
        score = curStatus.updateScore(score, pruning, curStatus.board, true)
        val resolveLog = curStatus.logString(pruning)
        if( (depth == curStatus.curDepth && !again) || endGame)
          println(resolveLog)
        curStatus.updateAlphaBeta(score, pruning, (!again && depth == curStatus.curDepth || endGame))
        val updateBoard = (status.curDepth <= 1 && curStatus.curDepth <= 1 )
        status.updateScore(score, pruning, curStatus.board, updateBoard)
        println(status.logString(pruning))
        status.updateAlphaBeta(score, pruning, false)
      }
    }
    status.board = status.bestResult
    status
  }
  //val rootPlayer = 0
  val rootPlayer = 1
  val otherPlayer = if(rootPlayer == 0) 1 else 0
  //val depth = 2
  val depth = 3
  //val pruning = false
  val pruning = true
  //var myBoard = Board("0 2 2 2 0; 0 2 2 2 0")
  //var myBoard = Board("0 3 3 3 0; 0 3 3 3 0")
  var myBoard = Board("0 10 4 6 12 5 0; 0 15 0 8 3 11 0")
  if( myBoard.allEmpty(rootPlayer)) {
    myBoard = collectStones(otherPlayer, myBoard)
    val score = myBoard.eval(rootPlayer)
  }
  else {
    /*
      myBoard = playGreedy(0, myBoard)
      myBoard.printBoard */
    var root = Node(playerID = rootPlayer, maxNode = true, name = "root", myBoard, curDepth = 0)  //root is player 0, next round should be player 1
    var output = playMiniMax(root, depth = depth, extra = false, pruning = pruning)

    output.bestResult.printBoard
  }

}
