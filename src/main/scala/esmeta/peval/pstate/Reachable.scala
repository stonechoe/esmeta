package esmeta.peval.pstate

import esmeta.state.*
import esmeta.state.util.{UnitWalker}
import scala.collection.mutable.{Set as MSet}
import esmeta.ir.Var

class Reachable private () extends UnitWalker {
  
  val addrs = MSet.empty[Addr]

  override def walk(id: Var): Unit = 

  override def walk(v: Value): Unit = v match
    case addr: Addr => 
    case Clo(func, captured) =>
    case PClo(func, captured) =>
    case Cont(func, captured, callStack) =>
    case PCont(func, captured, callStack) =>
    case AstValue(ast) =>
    case GrammarSymbol(name, params) =>
    case esmeta.state.Math(decimal) =>
    case Infinity(pos) =>
    case esmeta.state.Enum(name) =>
    case CodeUnit(c) =>
    case esmeta.state.Number(double) =>
    case esmeta.state.BigInt(bigInt) =>
    case Str(str) =>
    case Bool(bool) =>
    case Undef =>
    case esmeta.state.Null =>
  
}

object Reachable {

  def from(addr: Predict[Value])(using pst : PState): Set[Addr] =

}