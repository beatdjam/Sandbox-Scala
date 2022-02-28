// 18 ミュータブルオブジェクト
// 18.1 どのようなオブジェクトがミュータブルなのか
// 変化する状態を持つオブジェクト = ミュータブル
// そのオブジェクト自体がミュータブルか+他のオブジェクトのミュータブルな操作を行っているか
// varで定義されていても、全く同じふるまいを見せるのであればそれはイミュータブルである
// (設定値のキャッシュなど)

// 18.2 再代入可能な変数とプロパティ
// オブジェクトのvarは暗黙的にgetter, setterができる
// getterはname, setterはname_になる
// オブジェクトのvarのスコープは宣言されたスコープのメソッドに見える
// 自動で定義されるメソッドと同名のものを定義すると振る舞いを変更できる

// 18.3 離散イベントシミュレーション
// 18.4 デジタル言語のための言語
// ケーブル
//class Wire
//val a,b,c = new Wire

// インバーター、ANDゲート、ORゲート
//def inverter(input: Wire, output: Wire)
//def andGate(a1: Wire, a2: Wire, output: Wire)
//def orGate(o1: Wire, o2: Wire, output: Wire)

// 半加算器
//def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire) = {
//  val d, e = new Wire
//  orGate(a, b, d)
//  andGate(a, b, c)
//  inverter(c, e)
//  andGate(d, e, s)
//}

// 全加算器
//def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire) = {
//  val s, c1, c2 = new Wire
//  halfAdder(a, cin, s, c1)
//  halfAdder(b, s, sum, c2)
//  orGate(c1, c2, cout)
//}

// 18.5 シミュレーションAPI
abstract class Simulation {
  type Action = () => Unit
  case class WorkItem(time: Int, action: Action)

  private var curtime = 0
  def currentTime: Int = curtime

  private var agenda: List[WorkItem] = List()

  private def insert(ag: List[WorkItem], item:WorkItem): List[WorkItem] = {
    if ( ag.isEmpty || item.time < ag.head.time) item :: ag
    else ag.head :: insert(ag.tail, item)
  }

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = WorkItem(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  private def next(): Unit = {
    (agenda: @unchecked) match {
      case item :: rest =>
        agenda = rest
        curtime = item.time
        item.action()
    }
  }

  def run(): Unit = {
    afterDelay(0) {
      println(s"*** simulation started, time = $currentTime ***")
    }

    while (agenda.nonEmpty) next()
  }
}

// 18.6 デジタル回路のシミュレーション
abstract class BasicCircuitSimulation extends Simulation {
  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int

  class Wire {
    private var sigVal = false
    private var actions: List[Action] = List()

    def getSignal = sigVal
    def setSignal(s: Boolean): Unit = if (s != sigVal) {
      sigVal = s
      actions foreach (_ ()) // (_ ()) は f => f()
    }
    def addAction(a: Action): Unit = {
      actions = a :: actions
      a()
    }
  }

  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) {
        output setSignal !inputSig
      }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire): Unit = {
    def andAction(): Unit = {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) {
        output setSignal (a1Sig & a2Sig)
      }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  def orGate(o1: Wire, o2: Wire, output: Wire): Unit = {
    def orAction(): Unit = {
      val o1Sig = o1.getSignal
      val o2Sig = o2.getSignal
      afterDelay(OrGateDelay) {
        output setSignal (o1Sig | o2Sig)
      }
    }
    o1 addAction orAction
    o2 addAction orAction
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"$name $currentTime new-value = ${wire.getSignal}")
    }
    wire addAction probeAction
  }
}

abstract class CircuitSimulation extends BasicCircuitSimulation {
  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire): Unit = {
    val d, e = new Wire
    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)
  }

  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire): Unit = {
    val s, c1, c2 = new Wire
    halfAdder(a, cin, s, c1)
    halfAdder(b, s, sum, c2)
    orGate(c1, c2, cout)
  }
}

object MySimulation extends CircuitSimulation {
  override def InverterDelay = 1
  override def AndGateDelay = 3
  override def OrGateDelay = 5
}

val input1, input2, sum, carry = new MySimulation.Wire
MySimulation.probe("sum", sum)
MySimulation.probe("carry", carry)
MySimulation.probe("input1", input1)
MySimulation.probe("input2", input2)

MySimulation.halfAdder(input1, input2, sum, carry)
input1 setSignal true
MySimulation.run()
input2 setSignal true
MySimulation.run()