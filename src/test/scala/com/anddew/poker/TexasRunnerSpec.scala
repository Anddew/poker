package com.anddew.poker

import java.io.{ByteArrayOutputStream, InputStream, OutputStream}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source


class TexasRunnerSpec extends AnyFlatSpec with Matchers {

  val in: InputStream = getClass.getResourceAsStream("/texas/submissions.txt")
  val out: OutputStream = new ByteArrayOutputStream()
  val expected: String = Source.fromResource("texas/results.txt").getLines().mkString("", "\n", "\n")


  "Submissions" should "be equals results" in {
    Console.withOut(out) {
      Console.withIn(in) {
        Runner.main(Array.empty)
      }
    }

    out.toString mustBe expected
  }

  // 26.476 sec
  // 26.465 sec
  // 24.300 sec
  // 23.233 sec
  // 24.198 sec
  // 23.641 sec

  // Added parallelism for hands
  // 10.749 sec
  // 10.623 sec
  // 10.757 sec
  // 11.729 sec
  // 10.578 sec

}
