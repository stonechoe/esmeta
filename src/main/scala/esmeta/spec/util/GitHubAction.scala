package esmeta.spec.util

import esmeta.*
import esmeta.spec.Spec
import org.jsoup.nodes.Element

/* GitHub action to warn novel yet steps */
object GitHubAction:

  extension (elem: Element)
    def startLine: Int = elem.sourceRange().start().lineNumber()
    def endLine: Int = elem.sourceRange().end().lineNumber()

  def warnYets(spec: Spec, ignoreYets: List[String]): Unit =
    for {
      algo <- spec.algorithms
      elem = algo.elem
      step <- algo.steps
      if (step.isInstanceOf[lang.YetStep])
      if (!ignoreYets.contains(step.toString(detail = true, location = false)))
    } do {
      printWarn(
        filename = "spec.html",
        // Not sure but they works when - 1, maybe both 1-based?
        line = elem.startLine + step.loc.getOrElse(???).start.line - 1, // TODO
        endLine = elem.startLine + step.loc.getOrElse(???).end.line - 1, // TODO,
        title = "Unknown Phrase Detected", // TODO
        message = "This is a newly-introduced syntax which ESMeta cannot understand. Type check in this algorithm will not be performed after this line."  // TODO
      )
    }

  /**
    * print workflow command for GitHub Actions
    *
    * @param filename
    * @param line 1-based
    * @param endLine
    */
  private def printWarn(
    filename: String,
    line: Int,
    endLine: Int,
    title: String,
    message: String
  ): Unit =
    println(s"::warning file=${filename},line=${line},endLine=${endLine},title=${title}::${message}")
    
  