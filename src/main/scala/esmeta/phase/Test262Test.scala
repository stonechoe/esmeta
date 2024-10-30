package esmeta.phase

import esmeta.{CommandConfig, TEST_MODE}
import esmeta.cfg.CFG
import esmeta.error.*
import esmeta.interpreter.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.{ConcurrentPolicy => CP}
import esmeta.util.SystemUtils.*
import esmeta.es.*
import esmeta.es.util.Coverage
import esmeta.test262.{*, given}
import esmeta.test262.util.TestFilter
import java.io.File
import java.util.concurrent.TimeoutException

// TODO sort imports
import esmeta.peval.util.Test262PEvalPolicy
import scala.util.{Try, Success, Failure}

/** `test262-test` phase */
case object Test262Test extends Phase[CFG, Summary] {
  val name = "test262-test"
  val help = "tests Test262 tests with harness files (default: tests/test262)."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Summary =

    val pevalConfig = config.peval.getOrElse(Test262PEvalPolicy.DEFAULT);

    if (config.coverage && !(pevalConfig.isNever)) then
      throw OptConflictError("-test262-test:coverage", "-test262-test:peval")

    // set test mode
    TEST_MODE = true

    // get target version of Test262
    val version = Test262.getVersion(config.target)
    val test262 = Test262(version, cfg, config.withYet)
    val targets =
      if (cmdConfig.targets.isEmpty) None
      else Some(cmdConfig.targets)

    if (config.timeLimit.isDefined && config.concurrent == CP.Auto)
      error(
        "Turing on both time limit option (-test262-test:timeout and " +
        "the concurrent mode (-test262-test:concurrent) with " +
        "automatic thread number is not allowed.",
      )

    // run test262 eval test
    val summary = test262.evalTest(
      targets,
      config.features,
      config.log,
      config.detail,
      config.progress,
      config.coverage,
      config.timeLimit,
      config.concurrent,
      pevalConfig,
    )

    // if summary has failed test case, throws an exception
    if (summary.failCount > 0) throw Test262Fail(summary.fail)

    // return summary
    summary

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "target",
      StrOption((c, s) => c.target = Some(s)),
      "set the target git version of Test262 (default: current version).",
    ),
    (
      "features",
      StrListOption((c, s) => c.features = Some(s)),
      "set the target features of Test262.",
    ),
    (
      "progress",
      BoolOption(_.progress = _),
      "show progress bar.",
    ),
    (
      "coverage",
      BoolOption(_.coverage = _),
      "measure node/branch coverage in CFG of ECMA-262.",
    ),
    (
      "timeout",
      NumOption((c, k) => c.timeLimit = Some(k)),
      "set the time limit in seconds (default: no limit).",
    ),
    (
      "with-yet",
      BoolOption(_.withYet = _),
      "test with currently ignored tests because of unknown issues.",
    ),
    (
      "log",
      BoolOption(_.log = _),
      "turn on logging mode.",
    ),
    (
      "detail-log",
      BoolOption((c, b) => { c.log ||= b; c.detail = b }),
      "turn on logging mode with detailed information.",
    ),
    (
      "concurrent",
      NumOption((c, k) =>
        c.concurrent =
          if (k <= 0) then CP.Auto else if (k == 1) CP.Single else CP.Fixed(k),
      ),
      "set the number of thread to use concurrently (default: no concurrent)." +
      " If number <= 0, use automatically determined number of threads.",
    ),
    (
      "peval",
      StrOption(
        (c, p) => {
          c.peval = Some(
            Test262PEvalPolicy
              .parseOpt(p)
              .getOrElse(throw OptInvalidError(p, "test262-test")),
          )
        },
        prefer = Test262PEvalPolicy.values.map(_.toParsedString),
      ),
      "turn on partial evaluation. (not possible with `coverage` option)",
    ),
  )
  case class Config(
    var target: Option[String] = None,
    var coverage: Boolean = false,
    var progress: Boolean = false,
    var timeLimit: Option[Int] = None,
    var withYet: Boolean = false,
    var log: Boolean = false,
    var detail: Boolean = false,
    var concurrent: CP = CP.Single,
    var features: Option[List[String]] = None,
    var peval: Option[Test262PEvalPolicy] = None,
  )
}
