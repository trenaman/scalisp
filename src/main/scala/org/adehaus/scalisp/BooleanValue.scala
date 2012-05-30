package org.adehaus.scalisp

case class BooleanValue(value: Boolean) extends Sexpr {
  override def asFloat = if (value) 1.0f else 0.0f

  override def asLong = if (value) 1l else 0l

  override def asString = value.toString

  override def asBoolean = value
}
