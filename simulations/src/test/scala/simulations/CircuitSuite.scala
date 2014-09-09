package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate test") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  test("orGate test") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)

    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "1 or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "1 or 0")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "1 or 1")
  }

  test("orGate2 test") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)

    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "1 or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "1 or 0")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "1 or 1")
  }

  test("Demux test") {
    val in = new Wire
    val c: List[Wire] = List.fill(2)(new Wire)
    val outs: List[Wire] = List.fill(4)(new Wire)

    demux(in, c, outs)

    in.setSignal(true)
    c(0).setSignal(false)
    c(1).setSignal(true)
    run

    assert(outs(0).getSignal === false, "out(0) is 0")
    assert(outs(1).getSignal === false, "out(1) is 0")
    assert(outs(2).getSignal === true, "out(2) is 1")
    assert(outs(3).getSignal === false, "out(3) is 0")

    c(0).setSignal(false)
    c(1).setSignal(false)
    run

    assert(outs(0).getSignal === true, "out(0) is 1")
    assert(outs(1).getSignal === false, "out(1) is 0")
    assert(outs(2).getSignal === false, "out(2) is 0")
    assert(outs(3).getSignal === false, "out(3) is 0")
  }
}
