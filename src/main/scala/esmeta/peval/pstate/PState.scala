package esmeta.peval.pstate

import esmeta.error.*
import esmeta.es.*
import esmeta.ir.*
import esmeta.peval.*
import esmeta.peval.pstate.*
import esmeta.state.*
import esmeta.spec.{Spec}
import esmeta.ty.*
import esmeta.util.BaseUtils.*
import scala.collection.mutable.{Map => MMap}
import scala.util.{Try, Success}

/** Partial-States for Specializer
  *
  * @param globals
  *   is usually same in every PState, since it is not intended to be modified
  * @param callStack
  * @param context
  * @param heap
  */
case class PState(
  val globals: Map[Global, Predict[Value]],
  var callStack: List[PCallContext],
  var context: PContext,
  val heap: PHeap,
) extends StateElem {

  self =>

  inline def func = context.func
  inline def locals = context.locals

  /** getter */
  def apply(rt: Predict[RefTarget])(using Spec): Predict[Value] = rt match
    case Known(rt) => apply(rt)
    case Unknown   => Unknown

  /** getter */
  def apply(rt: RefTarget)(using Spec): Predict[Value] =
    rt match
      case VarTarget(x)             => apply(x)
      case FieldTarget(base, field) => apply(base, field)

  /** variable getter */
  def apply(x: Var): Predict[Value] = x match
    case x: Global => globals.getOrElse(x, Unknown)
    case x: Local  => locals.getOrElse(x, throw UnknownVar(x))

  /** field getter */
  def apply(base: Value, field: Value)(using Spec): Predict[Value] =
    base match
      case addr: Addr    => heap(addr, field)
      case AstValue(ast) => Known(AstValue(ast(field)))
      case Str(str)      => apply(str, field)
      case v             => throw InvalidRefBase(v)

  /** string field getter */
  def apply(str: String, field: Value): Predict[Value] = field match
    case Math(k) => Known(CodeUnit(str(k.toInt)))
    case _       => throw WrongStringRef(str, field)

  /** address getter */
  def apply(addr: Addr): Predict[PObj] = heap(addr)

  /** define variables */
  def define(x: Var, value: Predict[Value]): Unit = x match
    case x: Global => /* do nothing */
    case x: Local  => locals += x -> value

  /** setter */
  def update(rt: RefTarget, value: Predict[Value]): Unit = rt match
    case VarTarget(x)             => update(x, value)
    case FieldTarget(base, field) => update(base, field, value)

  /** variable setter */
  def update(x: Var, value: Predict[Value]): Unit = x match
    case x: Global => /* do nothing */
    case x: Local  => locals += x -> value

  /** field setter */
  def update(base: Value, field: Value, value: Predict[Value]): PState = ???

  /** allocate a record object */
  def allocRecord(
    addr: Addr,
    tname: String,
    pairs: Iterable[(String, Predict[Value])] = Nil,
  ): Unit = heap.allocRecord(addr, tname, pairs)

  /** allocate a map object */
  def allocMap(addr: Addr, pairs: Iterable[(Value, Predict[Value])]): Unit =
    ???

  /** allocate a list object */
  def allocList(addr: Addr, vs: Iterable[Predict[Value]]): Unit =
    heap.allocList(addr, vs)

  def copied: PState = PState(
    globals,
    callStack,
    context.copied,
    heap.copied,
  )

  def join(other: PState): PState = /* TODO : join states */ {
    assert(
      self.callStack.map(_.ctxt.func.name) == other.callStack.map(
        _.ctxt.func,
      ),
    )
    assert(
      self.context.sensitivity == other.context.sensitivity,
    )
    val newHeap = PHeap {
      val m = MMap.empty[Addr, Predict[PObj]]
      val keys = self.heap.map.keys ++ other.heap.map.keys
      for (k <- keys) {
        val x = self.heap.map.get(k)
        val y = other.heap.map.get(k)
        (x, y) match
          case (None, None)                           => /* never happens */
          case (Some(pv), None)                       => m += k -> pv
          case (None, Some(pv))                       => m += k -> pv
          case (Some(pv1), Some(pv2)) if (pv1 == pv2) => m += k -> pv1
          case _                                      => m += k -> Unknown
      }
      m
    }

    val newLocals = {
      val m = MMap.empty[Local, Predict[Value]]
      val keys = self.context.locals.keys ++ other.context.locals.keys
      for (k <- keys) {
        val x = self.context.locals.get(k)
        val y = other.context.locals.get(k)
        (x, y) match
          case (None, None)                           => /* never happens */
          case (Some(pv), None)                       => m += k -> pv
          case (None, Some(pv))                       => m += k -> pv
          case (Some(pv1), Some(pv2)) if (pv1 == pv2) => m += k -> pv1
          case _                                      => m += k -> Unknown
      }
      m
    }

    val newCtx = PContext(
      self.func,
      self.context.sensitivity, // ???
      newLocals,
      (self.context.ret, other.context.ret) match
        case (None, None) => None
        case (_, _)       => Some(Unknown),
    )
    PState(globals, callStack, newCtx, newHeap)
  }
}

object PState {
  def empty(ctx: PContext): PState = PState(
    globals = Map.empty, // temp fix // globals,
    callStack = Nil,
    context = ctx,
    heap = PHeap(),
  )
}
