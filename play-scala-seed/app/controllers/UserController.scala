package controllers

import controllers.UserController.{UserForm, userForm}
import models.Tables._
import play.api.data.Forms._
import play.api.data._
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.mvc._
import slick.jdbc.H2Profile.api._
import slick.jdbc.JdbcProfile

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

// ・Playではコントローラはクラスとして実装します
// ・@InjectはDIのためのアノテーションです
// 国際化機能を使用するにはコントローラにMessagesControllerComponentsをDIし、MessagesAbstractControllerクラスを継承します
// ・TODOメソッドはAction not implemented yet.という501 NOT_IMPLEMENTEDレスポンスを返します
class UserController @Inject()(implicit ec: ExecutionContext, val dbConfigProvider: DatabaseConfigProvider, components: MessagesControllerComponents)
  extends MessagesAbstractController(components) with HasDatabaseConfigProvider[JdbcProfile] {

  /**
   * 一覧表示
   */
  // IDの昇順にすべてのユーザ情報を取得
  // 一覧画面を表示
  def list: Action[AnyContent] = Action.async { implicit rs =>
    db.run(Users.sortBy(t => t.id).result)
      .map { users => Ok(views.html.users.list(users)) }
  }

  /**
   * 編集画面表示
   */
  def edit(id: Option[Long]): Action[AnyContent] = Action.async { implicit rs =>
    // リクエストパラメータにIDが存在する場合
    val form = if (id.isDefined) {
      // IDからユーザ情報を1件取得
      // 値をフォームに詰める
      db.run(Users.filter(t => t.id === id.get.bind).result.head)
        .map { user => userForm.fill(UserForm(Some(user.id), user.name, user.companyId)) }
    } else Future(userForm) // リクエストパラメータにIDが存在しない場合

    // 会社一覧を取得
    form.flatMap { form =>
      db.run(Companies.sortBy(_.id).result)
        .map { companies => Ok(views.html.users.edit(form, companies)) }
    }
  }

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

object UserController {
  // フォームの値を格納するケースクラス
  case class UserForm(id: Option[Long], name: String, companyId: Option[Int])

  // formから送信されたデータ ⇔ ケースクラスの変換を行う
  val userForm = Form(
    mapping(
      "id" -> optional(longNumber),
      "name" -> nonEmptyText(maxLength = 20),
      "companyId" -> optional(number)
    )(UserForm.apply)(UserForm.unapply)
  )
}