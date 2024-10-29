package esmeta.peval.util

import esmeta.ir.*
import esmeta.ir.util.{UnitWalker}
import scala.collection.mutable.{Set as MSet}
import esmeta.compiler.TyCompiler.walk

/** Remove unused definitions from the program, using syntactic approach */
class RemoveUnusedDef private () {

  self =>
  private class RightSideVarWalker() extends UnitWalker {

    val vars = MSet.empty[Var];

    def yieldVars: Set[Var] = Set.from(vars)
    override def walk(x: Var): Unit = vars += x

    override def walk(ref: Ref): Unit = ref match
      case Field(base, expr) => walk(base); walk(expr)
      case rest: Var         => vars += rest

    override def walk(x: Name): Unit = vars += x
    override def walk(inst: Inst): Unit = inst match
      case IExpr(expr)   => walk(expr)
      case ILet(_, expr) => walk(expr)
      case IAssign(ref, expr) =>
        if ref.isPure then () else walk(ref); walk(expr)
      case IExpand(base, expr)      => walk(base); walk(expr)
      case IDelete(base, expr)      => walk(base); walk(expr)
      case IPush(elem, list, front) => walk(elem); walk(list); walk(front)
      case IPop(_, list, front)     => walk(list); walk(front)
      case IReturn(expr)            => walk(expr)
      case IAssert(expr)            => walk(expr)
      case IPrint(expr)             => walk(expr)
      case INop()                   => ()
      case IIf(cond, thenInst, elseInst) =>
        walk(cond); walk(thenInst); walk(elseInst)
      case IWhile(cond, body)    => walk(cond); walk(body)
      case ICall(_, fexpr, args) => walk(fexpr); args.foreach(walk)
      case ISdoCall(_, base, op, args) =>
        walk(base); walk(op); args.foreach(walk)
      case ISeq(insts) => insts.foreach(walk)

  }

  def prune(inst: Inst): Inst =
    val rightSideVars = {
      val walker = new RightSideVarWalker()
      walker.walk(inst)
      walker.yieldVars
    }
    def aux(inst: Inst): Inst = inst match
      case ILet(lhs, expr) =>
        if (!rightSideVars.contains(lhs)) then ISeq(Nil)
        else inst
      case IAssign(ref, expr) =>
        ref match
          // v :Var means ref.isPure == true
          case v: Var if (!rightSideVars.contains(v)) => ISeq(Nil)
          case _                                      => inst
      case IExpand(base, expr)           => inst
      case IDelete(base, expr)           => inst
      case IPush(elem, list, front)      => inst
      case IPop(lhs, list, front)        => inst
      case ICall(lhs, fexpr, args)       => inst
      case ISdoCall(lhs, base, op, args) => inst
      case ISeq(insts)                   => ISeq(insts.map(aux)).passCmt(inst)
      // no recursion for now
      case IIf(cond, thenInst, elseInst) =>
        inst // IIf(cond, prune(thenInst), prune(elseInst)).passCmt(inst) else inst
      case IWhile(cond, body) => inst // IWhile(cond, prune(body)).passCmt(inst)
      case _                  => inst
    aux(inst)

}

object RemoveUnusedDef {
  def apply(inst: Inst): Inst = new RemoveUnusedDef().prune(inst)
}
