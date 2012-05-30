package org.adehaus.scalisp

case class StringValue(value: String) extends Sexpr {
  override def asFloat = value.toFloat
  override def asLong = value.toLong
  override def asString = value
  override def asBoolean = value.equalsIgnoreCase("true") || value.equalsIgnoreCase("1")
}
