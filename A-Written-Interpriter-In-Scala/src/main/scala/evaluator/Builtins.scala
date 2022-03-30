package evaluator

import `object`.{Array, Builtin, Error, Integer, Null, Object, Str}

object Builtins {
  val defines = Map(
    "len" -> Builtin((args: Seq[Object]) =>
      args match {
        case _ if args.length != 1 =>
          getError(s"wrong number of arguments. got=${args.length}, want=1")
        case Seq(Str(value), _*) =>
          Some(Integer(value.length))
        case Seq(Array(elements), _*) =>
          Some(Integer(elements.length))
        case value @ _ =>
          val objectType =
            value.headOption.map(_.objectType).getOrElse("unknown")
          getError(s"argument to len not supported, got $objectType")
      }
    ),
    "first" -> Builtin((args: Seq[Object]) =>
      args match {
        case _ if args.length != 1 =>
          getError(s"wrong number of arguments. got=${args.length}, want=1")
        case Seq(Str(value), _*) =>
          Some(
            value.headOption.map(char => Str(char.toString)).getOrElse(Null())
          )
        case Seq(Array(elements), _*) =>
          Some(elements.headOption.getOrElse(Null()))
        case value @ _ =>
          val objectType =
            value.headOption.map(_.objectType).getOrElse("unknown")
          getError(s"argument to first not supported, got $objectType")
      }
    ),
    "last" -> Builtin((args: Seq[Object]) =>
      args match {
        case _ if args.length != 1 =>
          getError(s"wrong number of arguments. got=${args.length}, want=1")
        case Seq(Str(value), _*) =>
          Some(
            value.reverse.headOption
              .map(char => Str(char.toString))
              .getOrElse(Null())
          )
        case Seq(Array(elements), _*) =>
          Some(elements.reverse.headOption.getOrElse(Null()))
        case value @ _ =>
          val objectType =
            value.headOption.map(_.objectType).getOrElse("unknown")
          getError(s"argument to last not supported, got $objectType")
      }
    ),
    "rest" -> Builtin((args: Seq[Object]) =>
      args match {
        case _ if args.length != 1 =>
          getError(s"wrong number of arguments. got=${args.length}, want=1")
        case Seq(Str(value), _*) =>
          val rest = value.tail
          if (rest.nonEmpty) Some(Str(rest)) else Some(Str(""))
        case Seq(Array(elements), _*) =>
          val rest = if (elements.nonEmpty) elements.tail else Nil
          if (rest.nonEmpty) Some(Array(rest))
          else Some(Array(Nil))
        case value @ _ =>
          val objectType =
            value.headOption.map(_.objectType).getOrElse("unknown")
          getError(s"argument to rest not supported, got $objectType")
      }
    ),
    "push" -> Builtin((args: Seq[Object]) =>
      args match {
        case Seq(Array(elements), obj) => Some(Array(elements :+ obj))
        case _ if args.isEmpty =>
          getError(s"wrong number of arguments. got=${args.length}")
        case value @ _ =>
          val objectType =
            value.headOption.map(_.objectType).getOrElse("unknown")
          getError(s"argument to push not supported, got $objectType")
      }
    ),
    "puts" -> Builtin((args: Seq[Object]) =>
      args match {
        case _ if args.isEmpty =>
          getError(s"wrong number of arguments. got=${args.length}")
        case _ =>
          args.foreach(arg => println(arg.inspect))
          None
      }
    )
  )

  private def getError(msg: String) = Some(Error(msg))
}
