package org.adehaus.scalisp

import util.parsing.combinator._

class LispParser extends JavaTokenParsers {

  def sexpr: Parser[Sexpr] = (
      "("~"+"~>rep(atom)<~")"       ^^ { Addition(_) }
    | "("~"-"~>rep(atom)<~")"       ^^ { Subtraction(_) }
    | "("~"and"~>rep(atom)<~")"     ^^ { And(_) }
    | "("~"or"~>rep(atom)<~")"      ^^ { Or(_) }
    | "("~"<"~>rep(atom)<~")"       ^^ { LessThan(_) }
    | "("~">"~>rep(atom)<~")"       ^^ { GreaterThan(_) }
    | "("~"if"~>rep(atom)<~")"      ^^ { If(_) }
    | "("~"set"~ident~atom~")"      ^^ {
        case "("~"set"~symbol~expr~")" => SetValue(symbol, expr)
      }
    | "("~"defun"~ident~variable_list~atom~")" ^^ {
        case "("~"defun"~name~vars~expr~")" => Defun(name, vars, expr)
      }
    | "("~ident~rep(atom)~")" ^^ {
        case "("~f~params~")" => FunctionCall(f, params)
      }
    | failure("Failure parsing s-expr")
  )

  def atom: Parser[Sexpr] = (
        ident ^^ { x: String =>  Symbol(x) }
      | wholeNumber ^^ { x: String => LongConstant(x.toLong) }
      | decimalNumber ^^ { x: String => FloatConstant(x.toFloat) }
      | stringLiteral ^^ { x: String => StringConstant(x.toString) }
      | sexpr
      | failure ("Failure parsing atom")
  )

  def variable_list: Parser[List[String]] = "("~>rep(ident)<~")"
}
