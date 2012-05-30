package org.adehaus.scalisp

case class Environment (map: Map[String, Sexpr], parent: Option[Environment] = None) {

  def add(keyValue: List[Pair[String, Sexpr]]): Environment = {
    Environment(map ++ keyValue, parent)
  }

  def get(key: String): Sexpr = {
    if (map.contains(key)) {
      map(key)
    } else if (parent.isDefined) {
      parent.get.get(key)
    } else {
      throw new RuntimeException("Unable to retrieve %s from environment.".format(key))
    }
  }

  override def toString = write("")

  def write(indent: String): String = {
    val values = map.map { case (key, value) => "%s%s = %s".format(indent, key, value) } mkString("\n")
    val parentString = parent.map(_.write("  ")).getOrElse("")
    "%s{\n%s\nparent: %s\n%s}".format(indent, values, parentString, indent)
  }
}

object EmptyEnvironment extends Environment(Map.empty[String, Sexpr])
