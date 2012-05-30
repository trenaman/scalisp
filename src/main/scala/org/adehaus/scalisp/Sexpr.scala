package org.adehaus.scalisp

trait Sexpr {
  def asFloat: Float = throw new RuntimeException("Not a Float")
  def asLong: Long  = throw new RuntimeException("Not a Long")
  def asString: String  = throw new RuntimeException("Not a String")
  def asBoolean: Boolean = throw new RuntimeException("Not a Boolean")
  def eval(env: Environment = EmptyEnvironment): (Sexpr, Environment) = (this, env)
}

case class Symbol(symbol: String) extends Sexpr {
  override def eval(env: Environment) = env.get(symbol).eval(env)
}

case class StringConstant(value: String) extends Sexpr {
  override def eval(env: Environment) = (StringValue(value), env)
}

case class LongConstant(value: Long) extends Sexpr {
  override def eval(env: Environment) = (LongValue(value), env)
}

case class FloatConstant(value: Float) extends Sexpr {
  override def eval(env: Environment) = (FloatValue(value), env)
}

case class Addition(args: List[Sexpr]) extends Sexpr {
  override def eval(env: Environment) = {
    val sum: Long = args.map(_.eval(env)._1.asLong).foldLeft(0l)(_+_)
    (LongValue(sum), env)
  }
}

case class Subtraction(args: List[Sexpr]) extends Sexpr {
  override def eval(env: Environment) = {
    val head = args.head.eval(env)._1.asLong
    val sumOfTail: Long = args.tail.map(_.eval(env)._1.asLong).foldLeft(0l)(_+_)
    (LongValue(head - sumOfTail), env)
  }
}

case class And(args: List[Sexpr]) extends Sexpr {
  override def eval(env: Environment) = {
    (BooleanValue(args.map(_.eval(env)._1.asBoolean).foldLeft(true)(_&&_)), env)
  }
}

case class Or(args: List[Sexpr]) extends Sexpr {
  override def eval(env: Environment) = {
    (BooleanValue(args.map(_.eval(env)._1.asBoolean).foldLeft(false)(_||_)), env)
  }
}

case class LessThan(args: List[Sexpr]) extends Sexpr {
  override def eval(env: Environment) = {
    assert(args.length == 2)
    (BooleanValue(args.head.eval(env)._1.asFloat < args.tail.head.eval(env)._1.asFloat), env)
  }
}

case class GreaterThan(args: List[Sexpr]) extends Sexpr {
  override def eval(env: Environment) = {
    assert(args.length == 2)
    (BooleanValue(args.head.eval(env)._1.asFloat > args.tail.head.eval(env)._1.asFloat), env)
  }
}

case class SetValue(symbol: String, sexpr: Sexpr) extends Sexpr {
  override def eval(env: Environment) = {
    val result: (Sexpr, Environment) = sexpr.eval(env)
    val newEnv = result._2.add((symbol, result._1) :: Nil)
    (result._1, newEnv)
  }
}

case class Defun(symbol: String, vars: List[String], sexpr: Sexpr) extends Sexpr {
  override def eval(env: Environment) = {
    val lambda = Lambda(symbol, vars, sexpr)
    val newEnv = env.add((symbol -> lambda) :: Nil)
    (lambda, newEnv)
  }
}

case class Lambda(symbol: String, vars: List[String], sexpr: Sexpr) extends Sexpr {
  override def eval(env: Environment) = {
    val parameters = (vars map { v  => (v, env.get(v))}).toMap
    val newEnv = Environment(parameters, Some(env))
    (sexpr.eval(newEnv)._1, env)
  }

  override def asString = this.toString
}

case class FunctionCall(f: String, args: List[Sexpr]) extends Sexpr {
  override def eval(env: Environment) = {
    val lambda = env.get(f).asInstanceOf[Lambda]
    val parameters = (lambda.vars.zip(args).map {case (v, e) => (v, e.eval(env)._1)}).toMap
    val newEnv = Environment(parameters, Some(env))
    (lambda.eval(newEnv)._1, env)
  }
}

case class If(args: List[Sexpr]) extends Sexpr {
  override def eval(env: Environment) = {
    assert(args.length == 3)
    val condition = args.head.eval(env)._1.asBoolean
    val resultExpr = if (condition) args.tail.head else args.tail.tail.head
    resultExpr.eval(env)
  }
}

case class Noop() extends Sexpr {
  override def eval(env: Environment) = (LongValue(0l), env)
}