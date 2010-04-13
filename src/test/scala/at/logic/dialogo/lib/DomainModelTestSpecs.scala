package at.logic.dialogo.lib

import org.specs.Specification
import org.specs.runner.{ConsoleRunner, JUnit3}
import xml.Unparsed

class DomainModelTestSpecsAsTest extends JUnit3(DomainModelTestSpecs)
object DomainModelTestSpecsRunner extends ConsoleRunner(DomainModelTestSpecs)

object DomainModelTestSpecs extends Specification {
  val formula = Implication(And(Operand("a"), Operand("b")), Operand("c"))
  val expectedHtml =
    <span id="expression5">(<span id="expression3">(<span id="expression1">a</span>{Unparsed("&#x2227;")}<span id="expression2">b</span>)</span>{Unparsed("&#x2192;")}<span id="expression4">c</span>)</span>

  "A domain model formula" should {
    "be representable as HTML" in {
      formula.toHtml must_== expectedHtml
    }
  }
}
