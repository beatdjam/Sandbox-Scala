package controllers

import play.api.db.slick._
import play.api.mvc.{AbstractController, ControllerComponents}
import slick.jdbc.JdbcProfile

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class JsonController @Inject()(protected val dbConfigProvider: DatabaseConfigProvider, cc: ControllerComponents)(
  implicit ec: ExecutionContext
) extends AbstractController(cc)
  with HasDatabaseConfigProvider[JdbcProfile] {

  /**
   * 一覧表示
   */
  def list = TODO

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