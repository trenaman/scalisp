package org.adehaus.scalisp

import util.parsing.combinator._

object Main extends LispParser {

  val Prompt = "scalisp> "

  def main(args: Array[String]) = {
    var result: Tuple3[Int, String, Environment] = null
    var env: Environment = EmptyEnvironment
    do {
      result = eval(readCmd, env)
      Console.println(result._2)
      env = result._3
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
    ":q to quit\n:h this message"
  }

  def eval(cmd: String, env: Environment): (Int, String, Environment) = {
    cmd.trim match {
      case ":q"         => (-1, "Bye Bye", env)
      case ":h"         => ( 0, helpMessage, env)
      case ":env"       => ( 0, env.toString, env)
      case trimmedCmd   => {
        val x: ParseResult[Sexpr] = parseAll(sexpr, trimmedCmd)

        if (x.successful) {
          val result = x.get.eval(env)
          (0, result._1.asString, result._2)
        } else {
          (0, "Parser error", env)
        }
      }
    }
  }

}
