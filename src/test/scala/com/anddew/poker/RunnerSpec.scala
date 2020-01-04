package com.anddew.poker

import java.io.{BufferedReader, ByteArrayOutputStream, InputStream, OutputStream, StringReader}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import scala.io.{Source, StdIn}


class RunnerSpec extends AnyFlatSpec with Matchers {

  val in: InputStream = getClass.getResourceAsStream("/submissions.txt")
  val out: OutputStream = new ByteArrayOutputStream()
  val expected: String = Source.fromResource("results.txt").getLines().mkString("\n")


  "Test input" should "be equals" in {
    Console.withOut(out) {
      Console.withIn(in) {
        Runner.main(Array.empty)
      }
    }

//    out.flush()

    out.toString mustBe expected
  }

}
