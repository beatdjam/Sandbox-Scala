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
      .map(users => Ok(views.html.users.list(users)))
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
        .map(user => userForm.fill(UserForm(Some(user.id), user.name, user.companyId)))
    } else Future(userForm) // リクエストパラメータにIDが存在しない場合

    // 会社一覧を取得
    form.flatMap { form =>
      db.run(Companies.sortBy(_.id).result)
        .map(companies => Ok(views.html.users.edit(form, companies)))
    }
  }

  /**
   * 登録実行
   */
  def create: Action[AnyContent] = Action.async { implicit rs =>
    // リクエストの内容をバインド
    userForm.bindFromRequest.fold(
      // エラーの場合
      error => {
        db.run(Companies.sortBy(t => t.id).result)
          .map(companies => BadRequest(views.html.users.edit(error, companies)))
      },
      // OKの場合
      form => {
        // ユーザを登録
        val user = UsersRow(0, form.name, form.companyId)
        // 一覧画面へリダイレクト
        db.run(Users += user)
          .map(_ => Redirect(routes.UserController.list))
      }
    )
  }

  /**
   * 更新実行
   */
  def update: Action[AnyContent] = Action.async { implicit rs =>
    // リクエストの内容をバインド
    userForm.bindFromRequest.fold(
      // エラーの場合は登録画面に戻す
      error => {
        db.run(Companies.sortBy(t => t.id).result)
          .map(companies => BadRequest(views.html.users.edit(error, companies)))
      },
      form => {
        // OKの場合は登録を行い一覧画面にリダイレクトする
        // ユーザ情報を更新
        val user = UsersRow(form.id.get, form.name, form.companyId)
        db.run(Users.filter(t => t.id === user.id.bind).update(user))
          .map(_ => Redirect(routes.UserController.list)) // 一覧画面にリダイレクト
      }
    )
  }

  /**
   * 削除実行
   */
  def remove(id: Long): Action[AnyContent] = Action.async { implicit rs =>
    // ユーザを削除
    db.run(Users.filter(t => t.id === id.bind).delete)
      .map(_ => Redirect(routes.UserController.list)) // 一覧画面へリダイレクト
  }

}

object UserController {
  // フォームの値を格納するケースクラス
  case class UserForm(id: Option[Long], name: String, companyId: Option[Int])

  // formから送信されたデータ ⇔ ケースクラスの変換を行う
  val userForm: Form[UserForm] = Form(
    mapping(
      "id" -> optional(longNumber),
      "name" -> nonEmptyText(maxLength = 20),
      "companyId" -> optional(number)
    )(UserForm.apply)(UserForm.unapply)
  )
}