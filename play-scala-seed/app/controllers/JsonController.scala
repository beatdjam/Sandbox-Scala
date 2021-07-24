package controllers

import play.api.db.slick._
import play.api.mvc.ControllerHelpers
import slick.jdbc.JdbcProfile

import javax.inject.Inject

class JsonController @Inject()(val dbConfigProvider: DatabaseConfigProvider) extends ControllerHelpers
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