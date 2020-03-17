package io.getquill.pftest

package partial

sealed trait Nest
case class Dual(one: Int, rest: Nest) extends Nest
case object End extends Nest

object PF {
  val ident: PartialFunction[Nest, String]  = PartialFunction.empty[Nest, String]

  trait Component extends (Root => PartialFunction[Nest, String]) {
    def name: String
    def apply(root: Root): PartialFunction[Nest, String]
  }
  abstract class Root { parentRoot =>
    def delegateParse(nest: Nest): String = {
      parsers.map(comp => comp(parentRoot)).foldRight(ident)(_ orElse _).apply(nest)
    }

    def parsers: List[Component] = List()
    def addParser(comp: Component): Root = {

      new Root { base =>
        override def parsers = parentRoot.parsers ++ List(comp)
      }
    }
  }
  object Root {
    //def empty: Root = single(Component.ident)

    def single(comp: Component): Root = {
      val newRoot = new Root {
        override def parsers = List(comp)
      }
      newRoot
    }
  }

  // tests
  // root apply of thing should be self
  // root apply vs apply of a composite?

//  val oneParser = new Component {
//    def name = "one"
//    override def parse: PartialFunction[Nest, String] = {
//      case Dual(1, rest) => "1:" + this.rootParse(rest)
//      case End => "!"
//    }
//  }
//  val twoParser = new Component {
//    def name = "two"
//    override def parse: PartialFunction[Nest, String] = {
//      case Dual(2, rest) => "2:" + this.rootParse(rest)
//    }
//  }

  class OneParser extends Component {
    def name = "one"
    override def apply(root: Root): PartialFunction[Nest, String] = {
      case Dual(1, rest) => "1:" + root.delegateParse(rest)
      case End => "!"
    }
  }
  class TwoParser extends Component {
    def name = "two"
    override def apply(root: Root): PartialFunction[Nest, String] = {
      case Dual(2, rest) => "2:" + root.delegateParse(rest)
    }
  }

  val root = Root.single(new OneParser).addParser(new TwoParser)



  def main(args: Array[String]):Unit = {
    val composite = root
    println(composite.parsers.map(_.name))
    println( composite.delegateParse(Dual(1, Dual(2, Dual(1, End)))) )
  }
}
