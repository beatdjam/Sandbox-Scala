package controllers

import models.Tables._
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.mvc._
import slick.jdbc.JdbcProfile
import slick.jdbc.H2Profile.api._

import javax.inject.Inject
import scala.concurrent.ExecutionContext

// ・Playではコントローラはクラスとして実装します
// ・@InjectはDIのためのアノテーションです
// 国際化機能を使用するにはコントローラにMessagesControllerComponentsをDIし、MessagesAbstractControllerクラスを継承します
// ・TODOメソッドはAction not implemented yet.という501 NOT_IMPLEMENTEDレスポンスを返します
class UserController @Inject()(implicit ec: ExecutionContext, val dbConfigProvider: DatabaseConfigProvider, components: MessagesControllerComponents)
  extends MessagesAbstractController(components) with HasDatabaseConfigProvider[JdbcProfile] {

  /**
   * 一覧表示
   */
  def list: Action[AnyContent] = Action.async { implicit rs =>
    // IDの昇順にすべてのユーザ情報を取得
    db.run(Users.sortBy(t => t.id).result).map { users =>
      // 一覧画面を表示
      Ok(views.html.users.list(users))
    }
  }

  /**
   * 編集画面表示
   */
  def edit(id: Option[Long]) = TODO

  /**
   * 登録実行
   */
  def create = TODO

  /**
   * 更新実行
   */
  def update = TODO

  /**
   * 削除実行
   */
  def remove(id: Long) = TODO

}