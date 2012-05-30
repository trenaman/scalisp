package org.adehaus.scalisp

import util.parsing.combinator._
import java.io.FileReader

object Main extends LispParser {

  val Prompt = "scalisp> "

  def main(args: Array[String]) = {
    var result: Tuple3[Int, String, Environment] = (0, "", EmptyEnvironment)
    var env: Environment = EmptyEnvironment
    do {
      try {
        result = eval(readCmd, env)
        Console.println(result._2)
        env = result._3
      } catch {
        case t: Throwable => Console.println(t.getMessage)
      }
    } while (result._1 ==  0)
  }

  def readCmd: String = {
    var cmd: String = null

    do {
      Console.print(Prompt)
      Console.flush
      cmd = Console.readLine()
    } while (cmd.isEmpty)

    cmd
  }

  def helpMessage: String = {
    """| :q               to quit
       | :h               this message
       | :env             show environment
       | :load <file>     load Lisp from a file""".stripMargin
  }

  def eval(cmd: String, env: Environment): (Int, String, Environment) = {
    cmd.trim match {
      case ":q"         => (-1, "Bye Bye", env)
      case ":h"         => ( 0, helpMessage, env)
      case ":env"       => ( 0, env.toString, env)
      case s if(s.startsWith(":load")) =>
        val fileName = s.substring(5).trim
        val file = new FileReader(fileName)
        evalParseResult(parseAll(sexpr, file), env)
      case trimmedCmd   => {
        evalParseResult(parseAll(sexpr, trimmedCmd), env)
      }
    }
  }

  def evalParseResult(x: ParseResult[Sexpr], env: Environment) : (Int, String, Environment) = {
    x match {
      case Success(result, _) =>
        val startTime = System.currentTimeMillis;
        val result = x.get.eval(env)
        val endTime = System.currentTimeMillis;
        (0, "%s (%dms)".format(result._1.asString, endTime - startTime), result._2)
      case fail: NoSuccess =>
        (0, "Parser error: %s".format(fail.msg), env)
    }
  }
}
