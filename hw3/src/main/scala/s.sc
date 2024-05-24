case class Board(val board: List[List[Player]], val player: Player) {

  type Line = List[Player]

  def transpose(b: List[List[Player]]): List[List[Player]] = {
    def aux(crt: Int, tempB: List[Line], acc: List[Line]): List[Line] = {
      if (crt == b.size) acc
      else {
        val heads = for (l <- tempB) yield l.head
        val rest = for (l <- tempB) yield l.tail
        aux(crt + 1, rest.filter(_ match { case Nil => false case _ => true }), heads :: acc)
      }
    }

    aux(0, b, Nil).reverse
  }

  def isFree(x: Int, y: Int): Boolean = board(x)(y) == Empty

  def getColumns: Board = new Board(transpose(board), player)

  def getFstDiag: Line = {
    //for (x <- List[Int].) yield this.board(x)(x)
    def aux(crt: Int, acc: Line): Line = {
      if (crt == -1) acc
      else aux(crt - 1, this.board(crt)(crt) :: acc)
    }

    if (this.board(0).size != 0) aux(this.board(0).size - 1, Nil)
    else Nil
  }

  def getSndDiag: Line = {
    //for (x <- List[Int].) yield this.board(x)(x)
    def aux(crt: Int, acc: Line): Line = {
      if (crt == -1) acc.reverse
      else aux(crt - 1, this.board(this.board(0).size - 1 - crt)(crt) :: acc)
    }

    if (this.board(0).size != 0) aux(this.board(0).size - 1, Nil)
    else Nil
  }


  def getLinesAboveFirstDiag: List[Line] = {
    def aux(crt: Int, target: Int, acc: Line): Line = {
      if (crt == target) acc
      else aux(crt + 1, target, acc.tail)
    }

    def sndAux(crt: Int, acc: List[Line]): List[Line] = {
      if (crt == board.size - 1) acc
      else {
        val auxResult = aux(0, crt + 1, this.board(crt))
        auxResult match {
          case Nil => sndAux(crt + 1, acc)
          case _ => sndAux(crt + 1, auxResult :: acc)
        }
      }
    }

    sndAux(0, Nil).reverse
  }

  def getAboveFstDiag: List[Line] = transpose(this.getLinesAboveFirstDiag)

  def getBelowFstDiag: List[Line] = new Board(transpose(board), this.player).getAboveFstDiag

  def getAboveSndDiag: List[Line] = new Board(this.board.map(_.reverse), player).getAboveFstDiag

  def getBelowSndDiag: List[Line] = new Board(this.board.map(_.reverse), player).getBelowFstDiag

  def lrDiag: List[Line] = getFstDiag :: getAboveFstDiag ::: getBelowFstDiag

  def rlDiag: List[Line] = getSndDiag :: getAboveSndDiag ::: getBelowSndDiag

  def allLines = (board ::: getColumns.board ::: lrDiag ::: rlDiag).filter(_.size > 4)

/*  def winner: Boolean = {
    def sndAux(lines: List[Line]): Boolean = {
      def winnerInLine(l: Line): Boolean = {
        def aux(inARow: Int, acc: Line, rest: List[Line]): Boolean = {
          if (inARow == 5) true
          else
            acc match {
              case Nil => aux(0, rest.head, rest.tail)
              case head :: tail =>
                if (head == player) aux(inARow + 1, tail)
                else                aux(0, tail)
            }
        }

        aux(0, l)
      }

      lines match {
        case Nil => false
        case head :: tail => if(aux(0, )) true
                             else sndAux(tail)
      }
    }

    sndAux(allLines)
  }*/
  /*
  def winner: Boolean = {
    def aux(inARow: Int, acc: Line, rest: List[Line]): Boolean = {
      if (inARow == 5) true
      else
        acc match {
          case Nil =>
            rest match {
              case Nil => false
              case _ => aux(0, rest.head, rest.tail)

          case head :: tail =>
            if (head == player) aux(inARow + 1, tail, rest)
            else aux(0, tail, rest)
        }
    }

    aux(0, board.head, board.tail)
  }*/


  def winner: Boolean = {
    def aux(inARow: Int, acc: Line): Boolean = {
        acc match {
          case Nil => inARow == 5
          case x =>
            if (inARow == 5) true
            else {
              if (acc.head == player.complement.complement) aux(inARow + 1, acc.tail)
              else aux(0, acc.tail)
            }
        }
    }

    //aux(0, board.head)
    allLines.exists(l => aux(0, l))
  }

  def winners: List[Int] = {
    def aux(inARow: Int, acc: Line): Int = {
      acc match {
        case Nil => inARow
        case x =>
          if (inARow == 5) 5
          else {
            if (acc.head == player.complement.complement) aux(inARow + 1, acc.tail)
            else aux(0, acc.tail)
          }
      }
    }

    //aux(0, board.head)
    allLines.map(l => aux(0, l))
  }

  def update(ln: Int, col: Int): Board = {
    val newBoard = (for (x <- 0 until board.size) yield
      (for (y <- 0 until board.size) yield
        if (x == ln && y == col) player.complement.complement
        else board(x)(y)).toList).toList

    Board(newBoard, player.complement.complement)
  }

/*  def update(ln: Int, col: Int): Board = {
    def aux(crt: Int, acc: List[Line], rest: List[Line]): List[Line] = {
      rest match {
        case Nil => acc
        case head :: tail  =>
          head match {
            case Nil => aux(crt+1, Nil :: acc, tail)
            case h :: t =>
              if (crt == ln * board.size + col) aux(crt+1, player :: acc, t :: tail)
              else                              aux(crt+1, h :: acc, t :: tail)
          }
      }
    }
    Board(aux(0, Nil, board), player)
  }*/

  def next: List[Board] = {
    val emptyCells =
      (for(i <- board.indices) yield
        (for(j <- board.indices) yield
          (i, j, board(i)(j) == Empty)).toList).toList

    val trullyEmptyCells = emptyCells.map(l => l.filter(c => c._3 == true))

    val iSwearItsTheLastVal =
      for(i <- trullyEmptyCells) yield
        for(j <- i) yield
          Board(board, player).update(j._1, j._2)

    def ofCourseAnotherAuxWhyNot(acc: List[Board], rest: List[List[Board]]): List[Board] = {
      rest match {
        case Nil          => acc
        case head :: tail => {
          head match {
            case Nil    => ofCourseAnotherAuxWhyNot(acc, tail)
            case h :: t => ofCourseAnotherAuxWhyNot(h :: acc, t :: tail)
          }
        }
      }
    }

    ofCourseAnotherAuxWhyNot(Nil, iSwearItsTheLastVal)
  }

/*
  def sequences: Map[Int, Int] = {
    val a =
      next.size

    val b =
      (for (n4 <- next)
        yield n4.next.size).sum

    val c =
      (for (n3 <- next) yield
        (for (n4 <- n3.next) yield
          n4.next.size).sum).sum

    val d =
      (for (n3 <- next) yield
        (for (n4 <- n3.next) yield
          (for (n5 <- n4.next) yield
            n5.next.size).sum).sum).sum


    Map((5, a), (4, b), (3, c), (2, d))
  }
*/


  def sequences: Map[Int, Int] = {
    val a = winners.count(_ == 5)

    val b = next.count(b => b.winner)

    val c =
      (for (n4 <- next)
        yield n4.next.filter(b => b.winner)).flatten.distinct.size

    val d =
      (for (n3 <- next) yield
        (for (n4 <- n3.next) yield
          n4.next.filter(b => b.winner))).flatten.flatten.distinct.size

    Map((5, a), (4, b), (3, c), (2, d))
  }


  def optimisedSequences: Map[Int, Int] = {
    val a = winners.count(_ == 5)

    val next1 = next
    val b = next1.count(b => b.winner)

    val next2 = for (n <- next1) yield n.next.partition(b => b.winner)
    val c = next2.flatMap(n => n._1).distinct.size

    val next3 = for (n <- next2.flatMap(m => m._2)) yield n.next.filter(b => b.winner)
    val d = next3.flatten.distinct.size

    Map((5, a), (4, b), (3, c), (2, d))
  }

  override def toString: String = {
    def toCos(p: Player): Char =
      p match {
        case One => 'X'
        case Two => '0'
        case Empty => '.'
      }

    def aux(rest: List[List[Char]], acc: String): String = {
      rest match {
        case Nil => acc
        case head :: tail => {
          head match {
            case Nil =>
              tail match {
                case Nil => acc
                case _ => aux(tail, acc + "\n")
              }
            case h :: t => aux(t :: tail, acc + h)
          }
        }
      }
    }

    aux(this.board.map(_.map(p => toCos(p))), "")
  }
}

object Board {

  def profileID:Int = 1

  def apply(s: String, p: Player): Board = new Board(apply(s).board, p.complement.complement)

  def apply(s: String): Board = {
    def toPos(c: Char): Player =
      c match {
        case 'X' => One
        case '0' => Two
        case _ => Empty
      }
    new Board(s.split('\n').toList.map(_.toList.map(c => toPos(c))), One)
  }
}


val small =
  """0.X
    |0X.
    |X..""".stripMargin.replace("\r\n", "\n")

val medium1 =
  """00000
    |0000X
    |00X..
    |00.0.
    |0X..0""".stripMargin.replace("\r\n", "\n")

val aboveFstDiag1 =
  """00..
    |00.
    |0X
    |0""".stripMargin.replace("\r\n", "\n")

val aboveSndDiag1 =
  """0000
    |000
    |00
    |0""".stripMargin.replace("\r\n", "\n")

val belowSndDiag1 =
  """X..X
    |.0.
    |..
    |0""".stripMargin.replace("\r\n", "\n")

val medium2 =
  """0X0X0.
    |000.X0
    |0.0X..
    |0..0..
    |0X..0X
    |...X..""".stripMargin.replace("\r\n", "\n")

val aboveFstDiag2 =
  """X0X.X
    |0...
    |XX.
    |00
    |.""".stripMargin.replace("\r\n", "\n")

val belowFstDiag2 =
  """0....
    |0..X
    |0X.
    |0.
    |.""".stripMargin.replace("\r\n", "\n")

val aboveSndDiag2 =
  """0.0.0
    |X0.0
    |000
    |X0
    |0""".stripMargin.replace("\r\n", "\n")

val belowSndDiag2 =
  """0.0..
    |....
    |.0X
    |X.
    |.""".stripMargin.replace("\r\n", "\n")


val smallUpd1 =
  """0XX
    |0X.
    |X..""".stripMargin.replace("\r\n", "\n")

val t2 =
  """X0X0X0
    |0X0X0X
    |X0X0X0
    |.XX0..
    |X00...
    |X0X0X0""".stripMargin

Board(small, One).update(0, 1).board == Board(smallUpd1).board

val smallUpd2 =
  """0.X
    |0X.
    |X.0""".stripMargin.replace("\r\n", "\n")

Board(small, Two).update(2, 2).board == Board(smallUpd2).board








val full =
  """0XX
    |0XX
    |XX0""".stripMargin

Board(full, Two).next == Nil
Board(full, One).next == Nil

val nextTest =
  """0..
    |0.X
    |.X.""".stripMargin

val nextTestR1 = Set("00.\n0.X\n.X.", "0.0\n0.X\n.X.", "0..\n00X\n.X.", "0..\n0.X\n0X.", "0..\n0.X\n.X0")
val nextTestR2 = Set("0X.\n0.X\n.X.", "0.X\n0.X\n.X.", "0..\n0XX\n.X.", "0..\n0.X\nXX.", "0..\n0.X\n.XX")

Board(nextTest, Two).next.map(_.toString).toSet == nextTestR1
Board(nextTest, One).next.map(_.toString).toSet == nextTestR2


val t1 =
  """......
    |......
    |......
    |..X...
    |......
    |......""".stripMargin
val t2 =
  """......
    |......
    |......
    |.XXX..
    |......
    |......""".stripMargin
val t3 =
  """......
    |......
    |......
    |0XXX..
    |......
    |......""".stripMargin

val t4 =
  """......
    |......
    |.000..
    |.XXX..
    |......
    |......""".stripMargin

val t5 =
  """......
    |......
    |.000..
    |.XXXX.
    |......
    |......""".stripMargin

Board(t1, One).sequences == Map(2 -> 0, 3 -> 0, 4 -> 0, 5 -> 0)
Board(t1, One).sequences
Board(t2, One).sequences(3)
Board(t3, One).sequences(3)
Board(t2, One).sequences(3) == 2
Board(t3, One).sequences(3) == 1

val nnn = 3