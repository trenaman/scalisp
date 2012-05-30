package org.adehaus.scalisp

case class LongValue(value: Long) extends Sexpr {
  override def asFloat = value.toFloat

  override def asLong = value

  override def asString = "%s".format(value)

  override def asBoolean = value == 1
}
