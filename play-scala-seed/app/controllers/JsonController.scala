package controllers

import controllers.JsonController._
import models.Tables
import models.Tables._
import play.api.db.slick._
import play.api.libs.functional.syntax.{toFunctionalBuilderOps, unlift}
import play.api.libs.json.{Json, OWrites, __}
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents}
import slick.jdbc.H2Profile.api._
import slick.jdbc.JdbcProfile

import javax.inject.Inject
import scala.concurrent.ExecutionContext


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
  def create = TODO

  /**
   * ユーザ更新
   */
  def update = TODO

  /**
   * ユーザ削除
   */
  def remove(id: Long) = TODO
}

object JsonController {
  // UsersRowをJSONに変換するためのWritesを定義
  implicit val usersRowWritesWrites: OWrites[Tables.UsersRow] = (
    (__ \ "id").write[Long] and
      (__ \ "name").write[String] and
      (__ \ "companyId").writeNullable[Int]
    ) (unlift(UsersRow.unapply))
}
