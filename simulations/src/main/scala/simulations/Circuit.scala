package simulations

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction(): Unit = {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) {output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
      val a1Inverted, a2Inverted, andOutput = new Wire
      inverter(a1, a1Inverted)
      inverter(a2, a2Inverted)
      andGate(a1Inverted, a2Inverted, andOutput)
      inverter(andOutput, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    def demux2to1(in: Wire, c: Wire, out0: Wire, out1: Wire): Unit = {
      val cInverted = new Wire
      inverter(c, cInverted)
      andGate(in, cInverted, out0)
      andGate(in, c, out1)
    }
    def demuxLayer(in: List[Wire], c: Wire, out: List[Wire]): Unit = {
      if (!in.isEmpty && !out.isEmpty && !out.tail.isEmpty) {
        demux2to1(in.head, c, out.head, out.tail.head)
        demuxLayer(in.tail, c, out.tail.tail)
      }
    }
    def demux(c: List[Wire], out: List[Wire]): Unit = {
      if (c.tail.isEmpty) {
        demuxLayer(List(in), c.head, out)
      } else {
        val ins: List[Wire] = List.fill(out.length/2)(new Wire)
        demuxLayer(ins, c.head, out)
        demux(c.tail, ins)
      }
    }
    demux(c, out)
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    println(">>> AND GATE <<<")
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  def orGateExample: Unit = {
    println(">>> OR GATE <<<")
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)

    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  def orGate2Example: Unit = {
    println(">>> OR GATE2 <<<")
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)

    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  def demuxExample: Unit = {
    println(">>> DEMUX 2:4 <<<")
    val in = new Wire
    val cs: List[Wire] = List.fill(2)(new Wire)
    val outs: List[Wire] = List.fill(4)(new Wire)

    demux(in, cs, outs)

    probe("in", in)
    cs.zipWithIndex.foreach { case (c, i) => probe("c[" + i + "]", c) }
    outs.zipWithIndex.foreach { case (out, i) => probe("out[" + i + "]", out) }

    in.setSignal(true)
    cs(0).setSignal(false)
    cs(1).setSignal(true)
    run

    cs(0).setSignal(false)
    cs(1).setSignal(false)
    run
  }
}

object CircuitMain extends App {
  Circuit.andGateExample
  Circuit.orGateExample
  Circuit.orGate2Example
  Circuit.demuxExample
}
