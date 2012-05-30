package org.adehaus.scalisp

case class FloatValue(val value: Float) extends Sexpr {
  override def asFloat = value
  override def asLong = value.toLong
  override def asString = "%f".format(value)
  override def asBoolean = this.asLong == 1
}
