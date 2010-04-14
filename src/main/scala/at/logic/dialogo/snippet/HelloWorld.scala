package at.logic.dialogo {
package snippet {

import _root_.scala.xml.{NodeSeq, Text}
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.java.util.Date
import at.logic.dialogo.lib._
import Helpers._

class HelloWorld {
  lazy val date: Box[Date] = DependencyFactory.inject[Date] // inject the date
  lazy val formula = Implication(Or(Operand("a"), Negation(Operand("a"))),
    And(Operand("b"), Operand("c")))

  def howdy(in: NodeSeq): NodeSeq =
    bind("b", in, "time" -> date.map(d => Text(d.toString)),
      "formulaHtml" -> formula.toHtml, "formulaCode" -> formula.toString)

  /*
   lazy val date: Date = DependencyFactory.time.vend // create the date via factory

   def howdy(in: NodeSeq): NodeSeq = bind("b", in, "time" -> date.toString)
   */
}

}
}
