package evaluator

import `object`.{Builtin, Error, Integer, Object, Str}

object Builtins {
  val defines = Map(
    "len" -> Builtin((args: Seq[Object]) =>
      if (args.length != 1) {
        Some(Error(s"wrong number of arguments. got=${args.length}, want=1"))
      } else {
        args.headOption.flatMap {
          case Str(value) => Some(Integer(value.length))
          case value @ _ =>
            Some(
              Error(s"argument to len not supported, got ${value.objectType}")
            )
        }
      }
    )
  )
}
