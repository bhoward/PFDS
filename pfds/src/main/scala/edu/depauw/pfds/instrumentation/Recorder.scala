package edu.depauw.pfds.instrumentation

class Recorder {
  private var ops: Int = 0
  private var ticks: Int = 0
  private var opTicks: Int = 0
  private var maxOpTicks: Int = 0
  private var maxAvgTicks: Int = 0
  private var maxOpTicksOp: Any = _
  private var maxAvgTicksOp: Any = _
  
  def tick[T](result: T): T = {
    opTicks += 1
    result
  }
  
  def ticks[T](count: Int)(result: T): T = {
    opTicks += count
    result
  }
  
//  def op[T](result: T): T = {
//    ops += 1
//    ticks += opTicks
//    maxOpTicks = maxOpTicks max opTicks
//    maxAvgTicks = maxAvgTicks max (ticks / ops)
//    opTicks = 0
//    result
//  }
  
  def op[T](op: Any)(result: T): T = {
    ops += 1
    ticks += opTicks
    if (opTicks > maxOpTicks) {
      maxOpTicks = opTicks
      maxOpTicksOp = op
    }
    if (ticks / ops > maxAvgTicks) {
      maxAvgTicks = ticks / ops
      maxAvgTicksOp = op
    }
    opTicks = 0
    result
  }
  
  def maximumTicksPerOp: Int = maxOpTicks
  
  def averageTicksPerOp: Int = ticks / ops
  
  def maximumAverageTicksPerOp: Int = maxAvgTicks
  
  def opWithMaxTicks: Any = maxOpTicksOp
  
  def opWithMaxAvgTicks: Any = maxAvgTicksOp
}