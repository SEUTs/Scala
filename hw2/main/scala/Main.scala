import scala.annotation.tailrec

abstract class WTree extends WTreeInterface {
  override def filter(pred: Token => Boolean): WTree = {
    filterAux(pred, Empty)
  }
  def filterAux(pred: Token => Boolean, acc: WTree): WTree

}

case object Empty extends WTree {
  override def balance: Int           = 0
  override def height: Int            = 0
  override def rotateLeft: WTree      = this
  override def rotateRight: WTree     = this
  override def rotateRightLeft: WTree = this
  override def rotateLeftRight: WTree = this
  override def rebalance: WTree       = this

  override def isEmpty                                              = true
  override def ins(w: Token): WTree                                 = Node(w, Empty, Empty)
  override def filterAux(pred: Token => Boolean, acc: WTree): WTree = acc
  override def size: Int                                            = 0
  override def contains(s: String): Boolean                         = false

}

case class Node(word: Token, left: WTree, right: WTree) extends WTree {

  override def balance: Int = right.height - left.height
  override def height: Int  = 1 + (left.height max right.height)
  override def rotateLeft: WTree =
    right match {
      case Node(w, l, r) => Node(w, Node(word, left, l), r)
    }
  override def rotateRight: WTree =
    left match {
      case Node(w, l, r) => Node(w, l, Node(word, r, right))
    }
  override def rotateRightLeft: WTree =
    Node(word, left, right.rotateRight).rotateLeft
  override def rotateLeftRight: WTree =
    Node(word, left.rotateLeft, right).rotateRight
  override def rebalance: WTree = {
    if (balance < -1 && left.balance == -1) this.rotateRight
    else if (balance > 1 && right.balance == 1) this.rotateLeft
    else if (balance < -1 && left.balance == 1) this.rotateLeftRight
    else if (balance > 1 && right.balance == -1) this.rotateRightLeft
    else this
  }

  override def isEmpty = false

  override def ins(w: Token): WTree =
    if (w.freq > word.freq) Node(word, left, right.ins(w))
    else Node(word, left.ins(w), right)

  override def contains(s: String): Boolean =
    if (word.word == s) true
    else if(left.contains(s)) true
    else
      if(right.contains(s)) true
      else
        false

  override def size: Int = 1 + this.left.size + this.right.size

  private def merge(left: WTree, right: WTree): WTree = left match {
    case Empty         => right
    case Node(w, l, r) => Node(w, merge(l, right), r)
  }
  def filterAux(pred: Token => Boolean, acc: WTree): WTree = {
    val leftFiltered  = left.filterAux(pred, Empty:WTree)
    val rightFiltered = right.filterAux(pred, Empty:WTree)
    if (pred(word)) Node(word, leftFiltered, rightFiltered)
    else merge(leftFiltered,rightFiltered)
  }
}

object Main {

  def profileID: Int = 884292

  val scalaDescription: String =
    "Scala is a strong statically typed general-purpose programming language which supports both object-oriented programming and functional programming designed to be concise many of Scala s design decisions are aimed to address criticisms of Java Scala source code can be compiled to Java bytecode and run on a Java virtual machine. Scala provides language interoperability with Java so that libraries written in either language may be referenced directly in Scala or Java code like Java, Scala is object-oriented, and uses a syntax termed curly-brace which is similar to the language C since Scala 3 there is also an option to use the off-side rule to structure blocks and its use is advised martin odersky has said that this turned out to be the most productive change introduced in Scala 3 unlike Java, Scala has many features of functional programming languages like Scheme, Standard ML, and Haskell, including currying, immutability, lazy evaluation, and pattern matching it also has an advanced type system supporting algebraic data types, covariance and contravariance, higher-order types (but not higher-rank types), and anonymous types other features of Scala not present in Java include operator overloading optional parameters named parameters and raw strings conversely a feature of Java not in Scala is checked exceptions which has proved controversial"

  def split(text: List[Char]): List[List[Char]] = {
    def aux(x: Char, acc: List[List[Char]]): List[List[Char]] =
      acc match {
        case Nil => if (x == ' ') Nil
                    else List(List(x))
        case head :: tail =>
          if(x == ' ') Nil :: head :: tail   // Nil :: acc
          else (x :: head) :: tail           // x :: acc
      }
    /*
    A,n,a, ,a,r,e, ,m,e,r,e
    Nil

    A,n,a, ,a,r,e, ,m,e,r,e
    List(List(e))                    ((e))

    A,n,a, ,a,r,e, ,m,e,r
    List(List(r,e))

    A,n,a, ,a,r,e, ,
    List(Nil, List(m,e,r,e))

    A,n,a, ,a,r,e
    List(List(e), List(m,e,r,e))

    A,n,a, ,a,r
    List(List(r,e), List(m,e,r,e))

    A,n,a, ,a
    List(List(a,r,e), List(m,e,r,e))
     */

    val l = text.foldRight(Nil: List[List[Char]])(aux)
    if (l == List(Nil)) Nil
    else l
  }

  def computeTokens(words: List[String]): List[Token] = {
    def insWord(s: String, acc: List[Token]): List[Token] = {
      val matching = acc.exists(_.word == s)

      if (!matching)
        acc.appended(Token(s, 1))
      else
        acc.map {
          case Token(word, freq) =>
            if (word == s)  Token(word, freq + 1)
            else            Token(word, freq)
        }
    }

    @tailrec
    def aux(rest: List[String], acc: List[Token]): List[Token] = {
      rest match {
        case Nil => acc
        case head :: tail => {
          val newAcc = insWord(head, acc)
          aux(tail, newAcc)
        }
      }
    }

    aux(words, Nil)
  }

  def tokensToTree(tokens: List[Token]): WTree = {
    tokens.foldLeft(Empty: WTree)((tree, token) => tree.ins(token))
    // de la stanga la dreapta, peste un copac gol    , adaugati toate token-urile
  }





  //      s             s.toList              words
  //"Ana are mere" => ('A','n',...) => (('A','n','a'),...) => ("Ana", "are", "mere")
  def makeTree(s: String): WTree = {
    val stringAsList = s.toList
    val words = split(stringAsList)
    val actualWords = words.map((chars: List[Char]) => chars.mkString)
    val finishedComputing = computeTokens(actualWords)
    tokensToTree(finishedComputing)
  }




  def wordSet: WTree =
    makeTree(scalaDescription)

  def scalaFreq: Int = {
    val doar = wordSet.filter((token) => token.word == "Scala")
    //doar = Node(Token("Scala", 5), Empty, Empty)
    doar match {
      case Node(token, left, right) => token.freq
      case Empty => 0
    }
  }

  def progLang: Int = {
    val doar = wordSet.filter((token) => token.word.charAt(0).isUpper)
    doar.size
  }

  def wordCount: Int = {
    val textAsChar = scalaDescription.toList
    val words = split(textAsChar)
    val filteredWords = words.filter(_.length > 3)
    filteredWords.size
  }
}
