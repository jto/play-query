package jto.play

import scala.{List => SList}
import scala.reflect.Manifest

import play.db.jpa._
import play.Logger

trait PlayScalaQueryImplicitConversions{
  implicit def symbol2Condition(n: Symbol): Condition = new ConditionFragment(Node(n))
  implicit def string2Node(s: String): Node = Node(s)
}

trait PlayQueryHelper extends PlayScalaQueryImplicitConversions{
  def getFirst[T <: Model]() = new PlaySingleResultQuery[T]
  def getAll[T <: Model]() = new PlayMultipleResultsQuery[T]
}

sealed abstract class PlayQuery[T <: Model](val conditions: Condition){
  def where(c: Condition): PlayQuery[T]
}

case class PlaySingleResultQuery[T <: Model](override val conditions: Condition) extends PlayQuery[T](conditions){
    def this() = this(NoCondition)
    override def where(c: Condition) = new PlaySingleResultQuery[T](c)
    def run()(implicit m: Manifest[T]): T = new QueryInvoker[T](this).execute().asInstanceOf[T]
}

case class PlayMultipleResultsQuery[T <: Model](override val conditions: Condition) extends PlayQuery[T](conditions){
   def this() = this(NoCondition)
   override def where(c: Condition) = new PlayMultipleResultsQuery[T](c)
   def run()(implicit m: Manifest[T]): List[T] = new QueryInvoker[T](this).execute().asInstanceOf[List[T]] 
}

/**
* build and execute the final HQL query
*/
class QueryInvoker[T <: Model](query: PlayQuery[T]) extends QueryOn[T]{
  //TODO: wrap in Option  
  def execute()(implicit m: Manifest[T]) = query match {
      case x: PlaySingleResultQuery[T] => get first
      case x: PlayMultipleResultsQuery[T] => get all
  }

  def get()(implicit m: Manifest[T]) = {
    val c = query.conditions.build
    Logger.info("generated query: [%s] with params: %s", c._1, c._2)
    find(c._1, c._2: _*)
  }

}

/**
* Query Nodes
*/
sealed class Node()
object Node{
  def apply(s: Symbol) = new SymbolNode(s)
  def apply(s: String) = new StringNode(s)
}
case class StringNode(val symbol: String) extends Node{
  override def toString() = symbol.toString
}
case class SymbolNode(val symbol: Symbol) extends Node{
  override def toString() = symbol.name
}

abstract class Condition extends Node{
  def build(): (String, SList[String])
  def is(value: Node) = Is(this, value)
  def and(right: Condition) = And(this, right)
  def or(right: Condition) = Or(this, right)
  def ~(right: Condition) = and(right)
}

object NoCondition extends Condition{
  override def build() = ("", SList())
}

class ConditionFragment(val left: Node) extends Condition{
  override def build = (left.toString, SList())
  override def toString = build._1
}

case class Is(val left: Node, val value: Node) extends Condition{
  override def build() = (left + " = ?", SList(value.toString))
}


abstract class Combinator(val left: Condition, val right: Condition) extends Condition{
   protected def combine(operator: String) = {
     val l = left.build
     val r = right.build

     val query = "(" + l._1 + " " + operator + " " + r._1 + ")"
     val params = l._2 ::: r._2
     (query, params)
   }
}

case class And(override val left: Condition, override val right: Condition) extends Combinator(left, right){
  override def build() = combine("and")
}

case class Or(override val left: Condition, override val right: Condition) extends Combinator(left, right){
  override def build() = combine("or")
}
