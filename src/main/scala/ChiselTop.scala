import chisel3._
import chisel3.util.Valid
import spire.algebra.Ring
import spire.algebra.Order
import spire.implicits._
import spire.math.{ConvertableFrom, ConvertableTo}

class Max[T <: Data: Ring: Order: ConvertableTo, V: ConvertableFrom](inGen:  T, outGen: T, count: Int)  extends Module {
  val io = IO(new Bundle{
    val input = Input(Valid(inGen));
    val max = Output(Valid(outGen));
  })

  private val last = Reg[T](inGen);
  private val counter = RegInit(0.U(32.U));

  when((io.input.bits.asUInt > last.asUInt) || (counter > count.U)) {
    last := io.input
    counter := 0.U //TODO: potentially there is a softer solution by just bit shifting a bit to left (dividing by 2)
  }

  counter := counter + 1.U
  io.max := last
  io.max.valid := io.input.valid
}

class AutoGainControl[T <: Data: Ring: Order: ConvertableTo, V: ConvertableFrom](inGen:  T, outGen: T)  extends Module {
  val io = IO(new Bundle {
    val input = Input(Valid(inGen))
    val output = Output(Valid(outGen))
  })

  val max = new Max[T, V](inGen, outGen, 1000);

  max.io.input := io.input;
  io.output := io.input.bits.asUInt / max.io.max.bits.asUInt;
  io.output.valid := max.io.max.valid;
}

class FIRFilter[T <: Data: Ring: Order: ConvertableTo, V: ConvertableFrom](length:  Int, scale: Int, inGen: T, outGen: T) extends Module {
  val io = IO(new Bundle {
    val input = Input(Valid(inGen))
    val output = Output(Valid(outGen))
    val consts = Input(Valid(Vec(1, inGen)))
    val index = Input(Valid(UInt(16.W)))
  })

  val factors = RegInit(VecInit(Seq.fill(length)(inGen)))

  when (io.consts.valid && io.index.valid) {
    factors := io.consts;
  }

  val taps = Seq(io.input.bits) ++ Seq.fill(factors.length - 1)(RegInit(0.S(8.W)))
  taps.zip(taps.tail).foreach { case (a, b) => when (io.input.valid) { b := a } }
  io.output := taps.zip(io.consts.bits).map { case (a, b) => (a.asUInt * b.asUInt >> (scale * scale)) }.reduce(_ + _)
}


class FFLBandEdge[T <: Data: Ring: Order: ConvertableTo, V: ConvertableFrom](bandwidth: Int, inGen:  T, outGen: T)  extends Module {
  val io = IO(new Bundle {
    val input = Input(Valid(inGen))
    val output = Output(Valid(outGen))
  })

  val fir = new FIRFilter[T, V](bandwidth , 8, inGen, outGen);

}

/**
 * Example design in Chisel.
 * A redesign of the Tiny Tapeout example.
 */
class ChiselTop() extends Module {
  val io = IO(new Bundle {
    val ui_in = Input(UInt(8.W))      // Dedicated inputs
    val uo_out = Output(UInt(8.W))    // Dedicated outputs
    val uio_in = Input(UInt(8.W))     // IOs: Input path
    val uio_out = Output(UInt(8.W))   // IOs: Output path
    val uio_oe = Output(UInt(8.W))    // IOs: Enable path (active high: 0=input, 1=output)
    val ena = Input(Bool())           // will go high when the design is enabled
  })

  io.uio_out := 0.U
  // use bi-directionals as input
  io.uio_oe := 0.U

  val add = WireDefault(0.U(7.W))
  add := io.ui_in + io.uio_in

  // Blink with 1 Hzq
  val cntReg = RegInit(0.U(32.W))
  val ledReg = RegInit(0.U(1.W))
  cntReg := cntReg + 1.U
  when (cntReg === 25000000.U) {
    cntReg := 0.U
    ledReg := ~ledReg
  }
  io.uo_out := ledReg ## add
}

object ChiselTop extends App {
  emitVerilog(new ChiselTop(), Array("--target-dir", "src"))
}