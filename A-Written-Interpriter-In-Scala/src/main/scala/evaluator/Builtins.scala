package evaluator

import `object`.{Array, Builtin, Error, Integer, Null, Object, Str}

object Builtins {
  val defines = Map(
    "len" -> Builtin((args: Seq[Object]) =>
      if (args.length != 1) {
        Some(Error(s"wrong number of arguments. got=${args.length}, want=1"))
      } else {
        args.headOption.flatMap {
          case Str(value)      => Some(Integer(value.length))
          case Array(elements) => Some(Integer(elements.length))
          case value @ _ =>
            Some(
              Error(s"argument to len not supported, got ${value.objectType}")
            )
        }
      }
    ),
    "first" -> Builtin((args: Seq[Object]) =>
      if (args.length != 1) {
        Some(Error(s"wrong number of arguments. got=${args.length}, want=1"))
      } else {
        args.headOption.flatMap {
          case Str(value) =>
            Some(
              value.headOption.map(char => Str(char.toString)).getOrElse(Null())
            )
          case Array(elements) =>
            Some(
              elements.headOption.getOrElse(Null())
            )
          case value @ _ =>
            Some(
              Error(s"argument to len not supported, got ${value.objectType}")
            )
        }
      }
    ),
    "last" -> Builtin((args: Seq[Object]) =>
      if (args.length != 1) {
        Some(Error(s"wrong number of arguments. got=${args.length}, want=1"))
      } else {
        args.headOption.flatMap {
          case Str(value) =>
            Some(
              value.reverse.headOption
                .map(char => Str(char.toString))
                .getOrElse(Null())
            )
          case Array(elements) =>
            Some(
              elements.reverse.headOption.getOrElse(Null())
            )
          case value @ _ =>
            Some(
              Error(s"argument to len not supported, got ${value.objectType}")
            )
        }
      }
    ),
    "rest" -> Builtin((args: Seq[Object]) =>
      if (args.length != 1) {
        Some(Error(s"wrong number of arguments. got=${args.length}, want=1"))
      } else {
        args.headOption.flatMap {
          case Str(value) =>
            val rest = value.tail
            if (rest.nonEmpty) Some(Str(rest))
            else Some(Null())
          case Array(elements) =>
            val rest = if (elements.nonEmpty) elements.tail else Nil
            if (rest.nonEmpty) Some(Array(rest))
            else Some(Null())
          case value @ _ =>
            Some(
              Error(s"argument to len not supported, got ${value.objectType}")
            )
        }
      }
    ),
    "push" -> Builtin((args: Seq[Object]) =>
      if (args.length != 2) {
        Some(Error(s"wrong number of arguments. got=${args.length}, want=2"))
      } else {
        args.head match {
          case Array(elements) =>
            Some(Array(elements :+ args(1)))
          case value @ _ =>
            Some(
              Error(s"argument to len not supported, got ${value.objectType}")
            )
        }
      }
    )
  )
}
