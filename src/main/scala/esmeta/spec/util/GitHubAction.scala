package esmeta.spec.util

import esmeta.*
import esmeta.spec.Spec
import org.jsoup.nodes.Element

/* GitHub action to warn novel yet steps */
object GitHubAction:

  extension (elem: Element)
    def startLine: Int = elem.sourceRange().start().lineNumber()
    def startCol: Int = elem.sourceRange().start().columnNumber()
    def endLine: Int = elem.sourceRange().end().lineNumber()
    def endCol: Int = elem.sourceRange().end().columnNumber()

  def warnYets(spec: Spec, ignoreYets: List[String]): Unit =
    for {
      algo <- spec.algorithms
      elem = algo.elem
      step <- algo.steps
      if (step.isInstanceOf[lang.YetStep])
      if (!ignoreYets.contains(step.toString(detail = true, location = false)))
    } do {
      printWarn(
        filename = Some("spec.html"),
        // -1, cause both 1-based
        line = Some(elem.startLine + step.loc.getOrElse(???).start.line - 1),
        endLine = Some(elem.startLine + step.loc.getOrElse(???).end.line - 1),
        col = Some(elem.startCol + step.loc.getOrElse(???).start.column - 1),
        endColumn = Some(elem.startCol + step.loc.getOrElse(???).end.column - 1),
        title = Some("Step Written in Unknown Phrase"), // TODO
        // TODO
        message = Some(s"""
        | This is a syntax which ESMeta cannot understand.
        | Type checking body of this algorithm (${algo.name}) will not be performed after this line.""".stripMargin.trim()
        )  // TODO
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
    filename: Option[String] = None,
    line: Option[Int]  = None,
    endLine: Option[Int]  = None,
    col: Option[Int] = None,
    endColumn: Option[Int] = None,
    title: Option[String] = None,
    message: Option[String] = None
  ): Unit =

    val args = List(
      "file" -> filename,
      "line" -> line,
      "endLine" -> endLine,
      "col" -> col,
      "endColumn" -> endColumn,
      "title" -> title
    ).flatMap { case (param, opt) => opt.map(param -> _) }
    .map { case (param, value) => s"$param=$value"}
    .mkString(",")

    println(s"::warning ${args}::${message.getOrElse("")}")
    
  