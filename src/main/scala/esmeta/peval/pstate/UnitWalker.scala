package esmeta.peval.pstate

import esmeta.state.*
import esmeta.state.util.{UnitWalker as StateUnitWalker}
import esmeta.ir.*
import esmeta.util.BasicUnitWalker

import esmeta.peval.{Predict, Known, Unknown}
import scala.annotation.targetName

/** unit walker for state */
trait UnitWalker extends StateUnitWalker {

  def walk(pst: PState): Unit =
    val PState(globals, callStack, context, heap) = pst
    walkMap(globals, walk, walk);
    walkList(callStack)
    walk(callStack);
    walk(context);
    walk(heap);

  def walk(pcc: PCallContext): Unit =
    val PCallContext(ctxt, redId) = pcc
    walk(ctxt)
    walk(redId)

  def walk(pctx: PContext): Unit =
    val PContext(func, sensitivity, locals, ret) = pctx
    walK(func)
    walk(sensitivity)
    walk(locals)
    walk(ret)

  @targetName("walkPV")
  def walk(pv: Predict[Value]): Unit = pv match
    case Known(v) => walk(v)
    case Unknown  =>

  @targetName("walkPRT")
  def walk(pr: Predict[RefTarget]): Unit = pr match
    case Known(tgt) => walk(tgt)
    case Unknown    =>

  // all cases
  def walk(elem: StateElem): Unit = elem match
    case elem: State       => walk(elem)
    case elem: Context     => walk(elem)
    case elem: Cursor      => walk(elem)
    case elem: CallContext => walk(elem)
    case elem: Heap        => walk(elem)
    case elem: Obj         => walk(elem)
    case elem: Value       => walk(elem)
    case elem: RefTarget   => walk(elem)
    case elem: Uninit      => walk(elem)

  // states
  def walk(st: State): Unit =
    val State(_, context, _, _, _, callStack, globals, heap) = st
    walk(context)
    walkList(callStack, walk)
    walkMMap(globals, walk, walk)
    walk(heap)

  // context
  def walk(context: Context): Unit =
    walkMMap(context.locals, walk, walk)
    context.retVal.map { case (_, value) => walk(value) }

  // cursor
  def walk(cursor: Cursor): Unit = {}

  // calling contexts
  def walk(callContext: CallContext): Unit = walk(callContext.context)

  // heap
  def walk(heap: Heap): Unit = walkMMap(heap.map, walk, walk)

  // object
  def walk(obj: Obj): Unit = obj match
    case RecordObj(_, map) => walkMMap(map, walk, walk)
    case MapObj(map)       => walkMMap(map, walk, walk)
    case ListObj(values)   => walkIterable(values, walk)
    case _: YetObj         =>

  // value
  def walk(v: Value): Unit = v match
    case addr: Addr       => walk(addr)
    case Clo(_, captured) => walkMap(captured, walk, walk)
    case Cont(_, captured, callStack) =>
      walkMap(captured, walk, walk); walkList(callStack, walk)
    case _: AstValue                   =>
    case _: GrammarSymbol              =>
    case _: Math                       =>
    case _: Infinity                   =>
    case _: Enum                       =>
    case _: CodeUnit                   =>
    case sv: SimpleValue               => walk(sv)
    case PClo(_, captrued)             => ???
    case PCont(_, captured, callStack) => ???

  // address
  def walk(addr: Addr): Unit = {}

  // simple value
  def walk(sv: SimpleValue): Unit = {}

  // reference value
  def walk(rv: RefTarget): Unit = rv match
    case _: VarTarget             =>
    case FieldTarget(base, field) => walk(base); walk(field)

  // ir id
  def walk(id: Var): Unit = {}

  // uninit value
  def walk(uninit: Uninit): Unit = {}
}
