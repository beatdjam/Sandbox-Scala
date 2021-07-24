package controllers

import controllers.JsonController._
import models.Tables
import models.Tables._
import play.api.db.slick._
import play.api.libs.functional.syntax.{toFunctionalBuilderOps, unlift}
import play.api.libs.json._
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents}
import slick.jdbc.H2Profile.api._
import slick.jdbc.JdbcProfile

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}


class JsonController @Inject()(implicit ec: ExecutionContext, val dbConfigProvider: DatabaseConfigProvider, cc: ControllerComponents)
  extends AbstractController(cc) with HasDatabaseConfigProvider[JdbcProfile] {

  /**
   * 一覧表示
   */
  def list: Action[AnyContent] = Action.async { implicit rs =>
    // IDの昇順にすべてのユーザ情報を取得
    db.run(Users.sortBy(t => t.id).result)
      .map { users => Ok(Json.obj("users" -> users)) } // ユーザの一覧をJSONで返す
  }

  /**
   * ユーザ登録
   */
  def create: Action[JsValue] = Action.async(parse.json) { implicit rs =>
    rs.body.validate[UserForm].map { form =>
      // OKの場合はユーザを登録
      val user = UsersRow(0, form.name, form.companyId)
      db.run(Users += user)
        .map { _ => Ok(Json.obj("result" -> "success")) }
    }.recoverTotal { e =>
      // NGの場合はバリデーションエラーを返す
      Future(BadRequest(Json.obj("result" -> "failure", "error" -> JsError.toJson(e))))
    }
  }

  /**
   * ユーザ更新
   */
  def update: Action[JsValue] = Action.async(parse.json) { implicit rs =>
    rs.body.validate[UserForm].map { form =>
      // OKの場合はユーザ情報を更新
      val user = UsersRow(form.id.get, form.name, form.companyId)
      db.run(Users.filter(t => t.id === user.id.bind)
        .update(user))
        .map { _ => Ok(Json.obj("result" -> "success")) }
    }.recoverTotal { e =>
      // NGの場合はバリデーションエラーを返す
      Future {
        BadRequest(Json.obj("result" -> "failure", "error" -> JsError.toJson(e)))
      }
    }
  }

  /**
   * ユーザ削除
   */
  def remove(id: Long): Action[AnyContent] = TODO
}

object JsonController {
  case class UserForm(id: Option[Long], name: String, companyId: Option[Int])

  // JSONをUserFormに変換するためのReadsを定義
  implicit val userFormFormat: Reads[UserForm] = (
    (__ \ "id").readNullable[Long] and
      (__ \ "name").read[String] and
      (__ \ "companyId").readNullable[Int]
    ) (UserForm)

  // UsersRowをJSONに変換するためのWritesを定義
  implicit val usersRowWritesWrites: OWrites[Tables.UsersRow] = (
    (__ \ "id").write[Long] and
      (__ \ "name").write[String] and
      (__ \ "companyId").writeNullable[Int]
    ) (unlift(UsersRow.unapply))
}
