package at.logic.dialogo.lib

import xml.Unparsed

object Formulas {
  var latestId = 0L

  def generateNextId = {
    latestId += 1
    "expression" + latestId
  }
}

trait AtomicExpression {
  val id = Formulas.generateNextId
  val isAtomic = true
  val atom: Atom

  def toHtml = <span id={id}>{Unparsed(atom.name)}</span>
}

abstract case class Expression(leftHandSide: AtomicExpression,
                               rightHandSide: AtomicExpression)
        extends AtomicExpression {
  override val isAtomic = false

  override def toHtml =
    <span id={id}>({leftHandSide.toHtml}{Unparsed(atom.name)}{rightHandSide.toHtml})</span>
}

case class Atom(name: String)

case class Operator(override val name: String, value: Any) extends Atom(name)

case class Operand(name: String) extends AtomicExpression {
  override val atom = Atom(name)
}

case class Falsum extends AtomicExpression {
  override val atom = Operator("&#x22A5;", false)
}

case class Verum extends AtomicExpression {
  override val atom = Operator("T", true)
}

case class Negation(rightHandSide: AtomicExpression) extends AtomicExpression {
  override val atom = Operator("&#x00AC;", "!")

  override def toHtml =
    <span id={id}>{Unparsed(atom.name)}({rightHandSide.toHtml})</span>
}

case class Implication(override val leftHandSide: AtomicExpression,
                       override val rightHandSide: AtomicExpression)
        extends Expression(leftHandSide, rightHandSide) {
  override val atom = Operator("&#x2192;", "->")
}

case class And(override val leftHandSide: AtomicExpression,
               override val rightHandSide: AtomicExpression)
        extends Expression(leftHandSide, rightHandSide) {
  override val atom = Operator("&#x2227;", "&")
}

case class SuperAnd(override val leftHandSide: AtomicExpression,
                    override val rightHandSide: AtomicExpression)
        extends Expression(leftHandSide, rightHandSide) {
  override val atom = Operator("&#x2299;", "&&")
}

case class Or(override val leftHandSide: AtomicExpression,
              override val rightHandSide: AtomicExpression)
        extends Expression(leftHandSide, rightHandSide) {
  override val atom = Operator("&#x2228;", "|")
}

case class SuperOr(override val leftHandSide: AtomicExpression,
                   override val rightHandSide: AtomicExpression)
        extends Expression(leftHandSide, rightHandSide) {
  override val atom = Operator("&#x2295;", "||")
}
