// クリーンアーキテクチャのメモ

// 出力の実体
class Presenter extends UseCaseOutputPort {
  def output(value : String): Unit = println(value)
}

// 出力の抽象
trait UseCaseOutputPort {
  def output(value : String): Unit
}

// 処理の実体
class UseCaseInteractor(presenter: UseCaseOutputPort) extends UseCaseInputPort {
  def handle(input: String): Unit = {
    presenter.output(input)
  }
}

// 処理の抽象
trait UseCaseInputPort {
  def handle(input: String): Unit
}

// 入力元
class Controller(service: UseCaseInputPort) {
  def doProcess(): Unit = service.handle("hoge")
}

val presenter = new Presenter()
val interactor = new UseCaseInteractor(presenter)
val controller = new Controller(interactor)
controller.doProcess()