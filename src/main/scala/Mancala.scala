/**
  * Created by Frankle on 2017/3/1.
  */
object Mancala extends App{
  case class Board(status: String = "0 4 4 4 4 4 4 4 0; 0 4 4 4 4 4 4 4 0") {
    var rows = status.split(";")
    val colSize = rows(0).split(" ").length

    var board = Array.ofDim[Int](2, colSize)

    for( i<- 0 to 1) {
      board(i) = rows(i).trim.split(" ").map(x => x.toInt)
    }

    def printBoard = {
      println(board.deep.mkString("\n"))
    }

    def move(playerID: Int, pitID: Int): Boolean = {
      var stones = board(playerID)(pitID)
      val numPits = colSize*2-2
      board(playerID)(pitID) = 0
      val offset = stones / numPits
      if( offset > 0) {
        for (i <- 0 to 1) {
          for (j <- 0 to colSize - 1) {
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
          board(curRow)(curPit) += 1
          if(curPit == colSize-1) {
            board(0)(curPit) = board(1)(curPit)
            curRow = 0
          }
        }
        else {
          curPit -= 1
          board(curRow)(curPit) += 1
          if( curPit == 0) {
            board(1)(0) = board(0)(0)
            curRow = 1
          }
        }
        stones -= 1
      }
      if( curRow == playerID && board(curRow)(curPit) == 1) {
        board(playerID)(if(playerID == 1) (colSize-1) else 0) += board(if(playerID == 1) 0 else 1)(curPit)
        board(if(playerID == 1) 0 else 1)(curPit) = 0
      }
      ( playerID == 1 && curPit == colSize-1 || playerID == 0 && curPit == 0 )  //whether to do a next move
    }
    def allEmpty(playerID: Int): Boolean = board(playerID).forall( x => x == 0)
    def eval(playerID: Int): Int = {
      if( playerID == 1) (board(playerID)(colSize-1) - board(0)(0)) else (board(0)(0) - board(playerID)(colSize-1))
    }

    override def toString: String = board(0).mkString(" ")+"; "+board(1).mkString(" ")
  }
  case class Node(playerID: Int, maxNode: Boolean, name: String, b: Board, curDepth: Int, extra: Boolean){
    var score = if(maxNode) Integer.MIN_VALUE else Integer.MAX_VALUE
    var board = b
    var this.curDepth = curDepth
    var this.name = name
    var this.playerID = playerID
    var otherPlayer = if(extra) playerID else if( playerID == 0) 1 else 0
    var bestResult: Board = b

    def evalScore: Int = board.eval(otherPlayer)
    def updateScore(newScore: Int, newBoard: Board, updateBoard: Boolean): Int = {
      if(maxNode && newScore > score || !maxNode && newScore < score) {
        score = newScore
        if( updateBoard)
          bestResult = Board(newBoard.toString)
      }
      score
    }
    def logString: String = {
      val scoreStr = if(score == Integer.MIN_VALUE) "-Infinity" else if(score == Integer.MAX_VALUE) "Infinity" else score.toString
      (this.name + "," + this.curDepth + "," + scoreStr)
    }
  }
  def collectStones(playerID: Int, b: Board): Board = {
    for(i <- 1 until b.colSize-1) {
      if( playerID == 0)
        b.board(0)(0) += b.board(0)(i)
      else
        b.board(1)(b.colSize-1) += b.board(0)(i)
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
  def playMiniMax(status: Node, depth: Int, extra: Boolean): Node = {
    //status is choosing among all possible outcomes of next (other player) round
    val playerID = status.playerID
    val otherPlayer = if(extra) playerID else if(playerID == 0) 1 else 0
    for( i <- 1 until status.b.colSize-1) {
      if( status.b.board(otherPlayer)(i) != 0) {
        val pitName = (if(otherPlayer == 0) "A" else "B") + (i+1)
        var curStatus = Node(otherPlayer, if(extra) status.maxNode else !status.maxNode, pitName, Board(status.b.toString), if(extra) status.curDepth else status.curDepth+1, extra)

        var again = curStatus.board.move(otherPlayer, i)
        val endGame = curStatus.board.allEmpty(otherPlayer)
        if( !endGame) {
          if (again)
            curStatus = Node(otherPlayer, curStatus.maxNode, curStatus.name, Board(curStatus.board.toString), curStatus.curDepth, again)
          if (depth != curStatus.curDepth || (depth == curStatus.curDepth && again)) {
            println(curStatus.logString)
          }
          if(again || depth != curStatus.curDepth) curStatus = playMiniMax(curStatus, depth, again)
        }
        else {
          curStatus = curStatus.copy(b = collectStones(otherPlayer, curStatus.board) )
        }
        if( extra && curStatus.curDepth == depth) {
          curStatus.otherPlayer = if (curStatus.otherPlayer == 0) 1 else 0
        }
        var score = if(!again && depth == curStatus.curDepth) curStatus.evalScore else curStatus.score
        score = curStatus.updateScore(score, curStatus.board, true)
        val resolveLog = curStatus.logString
        if( (depth == curStatus.curDepth && !again) || endGame)
          println(resolveLog)
        val updateBoard = (status.curDepth <= 1 && curStatus.curDepth <= 1 )
        status.updateScore(score, curStatus.board, updateBoard )
        println(status.logString)
      }
    }
    status.board = status.bestResult
    status
  }
  val playerID = 1
  val otherPlayer = if(playerID == 0) 1 else 0
  //var myBoard = Board("0 2 2 2 0; 0 2 2 2 0")
  var myBoard = Board("0 3 3 3 0; 0 3 3 3 0")
  if( myBoard.allEmpty(playerID)) {
    myBoard = collectStones(otherPlayer, myBoard)
    val score = myBoard.eval(playerID)
  }
  else {
    /*
      myBoard = playGreedy(0, myBoard)
      myBoard.printBoard */
    var root = Node(playerID = 0, maxNode = true, name = "root", myBoard, curDepth = 0, extra = false)  //root is player 0, next round should be player 1
    var output = playMiniMax(root, depth = 2, extra = false)

    output.bestResult.printBoard
  }

}
