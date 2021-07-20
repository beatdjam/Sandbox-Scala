package controllers

import play.api.db.slick._
import play.api.mvc._
import play.mvc.Controller
import slick.jdbc.JdbcProfile

import javax.inject.Inject


// ・Playではコントローラはクラスとして実装します
// ・@InjectはDIのためのアノテーションです
// 国際化機能を使用するにはコントローラにMessagesControllerComponentsをDIし、MessagesAbstractControllerクラスを継承します
// ・TODOメソッドはAction not implemented yet.という501 NOT_IMPLEMENTEDレスポンスを返します
class UserController @Inject()(components: MessagesControllerComponents)
  extends MessagesAbstractController(components) {

  /**
   * 一覧表示
   */
  def list = TODO

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