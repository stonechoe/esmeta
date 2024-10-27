package esmeta.peval.util

import esmeta.error.{PEvalOptError}
import scala.util.{Try, Success, Failure}

enum Test262PEvalStrategy:
  case Never, ComputeOnly, ComputeAndUse
  // case ComputeAndSimplifyThenUse(simplifyLevel: Int)

  def isNever = this match
    case Never => true
    case _     => false

  def shouldCompute = this match
    case ComputeOnly | ComputeAndUse => true
    case _                           => false

  def shouldUse = this match
    case ComputeAndUse => true
    case _             => false

object Test262PEvalStrategy:
  def from(s: String): Test262PEvalStrategy = s match
    case "never"           => Never
    case "compute-only"    => ComputeOnly
    case "compute-and-use" => ComputeAndUse
    // case "compute-and-simplify-then-use" => ComputeAndSimplifyThenUse(0)
    case _ => throw new PEvalOptError(s"Unknown PEval strategy: $s")

  def fromOpt(s: String): Option[Test262PEvalStrategy] = Try { from(s) } match
    case Success(v) => Some(v)
    case Failure(_) => None
