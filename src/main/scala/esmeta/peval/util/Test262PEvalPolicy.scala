package esmeta.peval.util

import esmeta.error.{PEvalOptError}
import scala.util.{Try, Success, Failure}

enum Test262PEvalPolicy:
  case Never
  case ComputeHarnessAndUseNothing, ComputeTargetAndUseNothing,
  ComputeBothAndUseNothing
  case ComputeHarnessAndUseHarness, ComputeBothAndUseHarness
  case ComputeTargetAndUseTarget, ComputeBothAndUseTarget
  case ComputeBothAndUseBoth
  // case ComputeAndSimplifyThenUse(simplifyLevel: Int)

  def isNever = this match
    case Never => true
    case _     => false

  def shouldComputeHarness = this match
    case ComputeHarnessAndUseNothing => true
    case ComputeBothAndUseNothing    => true
    case ComputeHarnessAndUseHarness => true
    case ComputeBothAndUseHarness    => true
    case ComputeBothAndUseTarget     => true
    case ComputeBothAndUseBoth       => true
    case _                           => false

  def shouldUseHarness = this match
    case ComputeHarnessAndUseHarness => true
    case ComputeBothAndUseHarness    => true
    case ComputeBothAndUseBoth       => true
    case _                           => false

  def shouldComputeTarget = this match
    case ComputeTargetAndUseNothing => true
    case ComputeBothAndUseNothing   => true
    case ComputeTargetAndUseTarget  => true
    case ComputeBothAndUseTarget    => true
    case ComputeBothAndUseBoth      => true
    case _                          => false

  def shouldUseTarget = this match
    case ComputeTargetAndUseTarget => true
    case ComputeBothAndUseTarget   => true
    case ComputeBothAndUseBoth     => true
    case _                         => false

object Test262PEvalPolicy:
  def from(s: String): Test262PEvalPolicy = s match
    case "never" => Test262PEvalPolicy.Never
    case "compute-harness-and-use-nothing" =>
      Test262PEvalPolicy.ComputeHarnessAndUseNothing
    case "compute-target-and-use-nothing" =>
      Test262PEvalPolicy.ComputeTargetAndUseNothing
    case "compute-both-and-use-nothing" =>
      Test262PEvalPolicy.ComputeBothAndUseNothing
    case "compute-harness-and-use-harness" =>
      Test262PEvalPolicy.ComputeHarnessAndUseHarness
    case "compute-both-and-use-harness" =>
      Test262PEvalPolicy.ComputeBothAndUseHarness
    case "compute-target-and-use-target" =>
      Test262PEvalPolicy.ComputeTargetAndUseTarget
    case "compute-both-and-use-target" =>
      Test262PEvalPolicy.ComputeBothAndUseTarget
    case "compute-both-and-use-both" => Test262PEvalPolicy.ComputeBothAndUseBoth
    case _ => throw PEvalOptError(s"Invalid Test262PEvalPolicy: $s")

  def fromOpt(s: String): Option[Test262PEvalPolicy] = Try { from(s) } match
    case Success(v) => Some(v)
    case Failure(_) => None
