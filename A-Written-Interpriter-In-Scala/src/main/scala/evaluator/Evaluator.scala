package evaluator

import `object`._
import ast.{
  ArrayLiteral,
  BlockStatement,
  BooleanExpression,
  CallExpression,
  Expression,
  ExpressionStatement,
  FunctionLiteral,
  HashLiteral,
  Identifier,
  IfExpression,
  IndexExpression,
  InfixExpression,
  IntegerLiteral,
  LetStatement,
  Node,
  PrefixExpression,
  Program,
  ReturnStatement,
  Statement,
  StringLiteral
}

object Evaluator {
  private val TRUE = Bool(true)
  private val FALSE = Bool(false)
  private val NULL = Null()

  def eval(node: Node, env: Environment): Option[Object] = {
    node match {
      case Program(statements) => evalProgram(statements, env)
      case ExpressionStatement(_, Some(expression)) =>
        eval(expression, env)
      case LetStatement(_, name, Some(expression)) =>
        val result = eval(expression, env)
        result match {
          case value @ Some(Error(_)) => value
          case Some(value) =>
            env.set(name.value, value)
            None
          case None => None
        }
      case BlockStatement(_, statements) =>
        evalBlockStatements(statements, env)
      case ReturnStatement(_, Some(returnValue)) =>
        eval(returnValue, env).map(Return)
      case PrefixExpression(_, operator, right) =>
        evalPrefixExpression(operator, eval(right, env))
      case InfixExpression(_, left, operator, right) =>
        evalInfixExpression(operator, eval(left, env), eval(right, env))
      case IfExpression(_, condition, consequence, alternative) =>
        eval(condition, env).flatMap { condition =>
          if (isTruthy(condition)) eval(consequence, env)
          else if (alternative.isDefined) alternative.flatMap(eval(_, env))
          else Some(NULL)
        }
      case FunctionLiteral(_, parameters, body) =>
        Some(Function(parameters, body, env))
      case CallExpression(_, function, arguments) =>
        evalCallExpression(env, function, arguments)
      case IntegerLiteral(_, value) =>
        Some(Integer(value))
      case StringLiteral(_, value) =>
        Some(Str(value))
      case BooleanExpression(_, value) =>
        Some(nativeBoolToBool(value))
      case Identifier(_, value) =>
        env.get(value) match {
          case Some(value) => Some(value)
          case None =>
            Builtins.defines.get(value) match {
              case result @ Some(_) => result
              case _                => Some(Error(s"identifier not found: $value"))
            }
        }
      case ArrayLiteral(_, elements) =>
        val evaluated = elements.flatMap(eval(_, env))
        Some(Array(evaluated))
      case IndexExpression(_, leftExp, indexExp) =>
        val left = eval(leftExp, env)
        val index = eval(indexExp, env)
        (left, index) match {
          case (Some(Array(elements)), Some(Integer(value))) =>
            if (elements.isDefinedAt(value)) Some(elements(value))
            else Some(NULL)
          case (Some(Hash(pairs)), Some(key)) =>
            key match {
              case key: Hashable =>
                pairs.get(key) match {
                  case Some(value) => Some(value)
                  case None        => Some(NULL)
                }
              case _ => Some(Error(s"unusable as hash key: ${key.objectType}"))
            }
          case _ =>
            Some(
              Error(
                s"index operator not supported: ${left.map(_.objectType).getOrElse("")}"
              )
            )
        }
      case HashLiteral(_, pairs) =>
        val evaluated = pairs
          .map { case (key, value) =>
            val keyValue = eval(key, env)
            val valueValue = eval(value, env)
            (keyValue, valueValue)
          }
          .map { case (Some(key), Some(value)) => key -> value }
        Some(Hash(evaluated))
      case _ => None
    }
  }

  private def evalCallExpression(
      env: Environment,
      function: Expression,
      arguments: Seq[Expression]
  ) = {
    def evalArgs = arguments.flatMap(eval(_, env))
    def extendFunctionEnv(
        parameters: Seq[Identifier],
        functionEnv: Environment
    ) = {
      val enclosedEnv = Environment.newEnclosedEnvironment(functionEnv)
      parameters
        .zip(evalArgs)
        .foreach { case (parameter, arg) =>
          enclosedEnv.set(parameter.value, arg)
        }
      enclosedEnv
    }
    def unwrapReturnValue(evaluatedFunction: Option[Object]) = {
      evaluatedFunction match {
        case Some(Function(parameters, body, functionEnv)) =>
          val extendedFunctionEnv = extendFunctionEnv(parameters, functionEnv)
          eval(body, extendedFunctionEnv) match {
            case Some(Return(value)) => Some(value)
            case result @ _          => result
          }
        case Some(Builtin(fn)) => fn(evalArgs)
        case _                 => None
      }
    }
    val evaluatedFunction = eval(function, env)
    unwrapReturnValue(evaluatedFunction)
  }

  private def evalStatements(
      statements: Seq[Option[Statement]],
      env: Environment
  ): Option[Object] = {
    val evaluated = statements.flatMap { _.flatMap(eval(_, env)) }
    val returnValue = evaluated.collectFirst {
      case result @ (Error(_) | Return(_)) => result
    }
    returnValue match {
      case result @ Some(_) => result
      case None             => evaluated.reverse.headOption
    }
  }

  private def evalProgram(
      statements: Seq[Option[Statement]],
      env: Environment
  ): Option[Object] = {
    evalStatements(statements, env) match {
      case Some(Return(value)) => Some(value)
      case result @ Some(_)    => result
      case None                => None
    }
  }

  private def evalBlockStatements(
      statements: Seq[Option[Statement]],
      env: Environment
  ): Option[Object] = {
    evalStatements(statements, env) match {
      case result @ Some(_) => result
      case None             => None
    }
  }

  private def nativeBoolToBool(input: Boolean): Bool =
    if (input) TRUE else FALSE

  private def evalPrefixExpression(
      operator: String,
      right: Option[Object]
  ): Option[Object] = {
    def evalMinusPrefixOperatorExpression(right: Option[Object]) = right match {
      case Some(Integer(value)) => Some(Integer(-value))
      case _ =>
        Some(
          Error(s"unknown operator: -${right.map(_.objectType).getOrElse("")}")
        )
    }

    def evalBangOperatorExpression(right: Option[Object]) = right match {
      case Some(TRUE)  => Some(FALSE)
      case Some(FALSE) => Some(TRUE)
      case Some(NULL)  => Some(TRUE)
      case _           => Some(FALSE)
    }

    operator match {
      case "!" => evalBangOperatorExpression(right)
      case "-" => evalMinusPrefixOperatorExpression(right)
      case _ =>
        Some(Error(s"unknown operator: $operator${right.map(_.objectType)}"))
    }
  }

  private def evalInfixExpression(
      operator: String,
      left: Option[Object],
      right: Option[Object]
  ): Option[Object] = {
    def evalIntegerInfixExpression(
        operator: String,
        leftValue: Int,
        rightValue: Int
    ): Option[Object] = operator match {
      case "+" => Some(Integer(leftValue + rightValue))
      case "-" => Some(Integer(leftValue - rightValue))
      case "*" => Some(Integer(leftValue * rightValue))
      case "/" => Some(Integer(leftValue / rightValue))
      case "<" => Some(Bool(leftValue < rightValue))
      case ">" => Some(Bool(leftValue > rightValue))
      case _ =>
        Some(
          Error(
            s"unknown operator: ${left
              .map(_.objectType)
              .getOrElse("")} $operator ${right.map(_.objectType).getOrElse("")}"
          )
        )
    }
    def evalStringInfixExpression(
        operator: String,
        leftValue: String,
        rightValue: String
    ): Option[Object] = operator match {
      case "+" => Some(Str(leftValue + rightValue))
      case _ =>
        Some(
          Error(
            s"unknown operator: ${left
              .map(_.objectType)
              .getOrElse("")} $operator ${right.map(_.objectType).getOrElse("")}"
          )
        )
    }

    (operator, left, right) match {
      case ("==", _, _) =>
        Some(nativeBoolToBool(left == right))
      case ("!=", _, _) =>
        Some(nativeBoolToBool(left != right))
      case (_, Some(Integer(leftValue)), Some(Integer(rightValue))) =>
        evalIntegerInfixExpression(operator, leftValue, rightValue)
      case (_, Some(Str(leftValue)), Some(Str(rightValue))) =>
        evalStringInfixExpression(operator, leftValue, rightValue)
      case (_, Some(leftValue), Some(rightValue)) =>
        if (leftValue.objectType != rightValue.objectType) {
          Some(
            Error(
              s"type mismatch: ${leftValue.objectType} $operator ${rightValue.objectType}"
            )
          )
        } else {
          Some(
            Error(
              s"unknown operator: ${leftValue.objectType} $operator ${rightValue.objectType}"
            )
          )
        }
      case _ =>
        Some(
          Error(
            s"unknown operator: ${left
              .map(_.objectType)
              .getOrElse("")} $operator ${right.map(_.objectType).getOrElse("")}"
          )
        )
    }
  }

  private def isTruthy(obj: Object): Boolean = obj match {
    case NULL  => false
    case TRUE  => true
    case FALSE => false
    case _     => true
  }
}
