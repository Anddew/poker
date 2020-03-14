package com.anddew.poker

import java.io.{ByteArrayOutputStream, InputStream, OutputStream}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source


class OmahaRunnerSpec extends AnyFlatSpec with Matchers {

  val in: InputStream = getClass.getResourceAsStream("/omaha/submissions.txt")
  val out: OutputStream = new ByteArrayOutputStream()
  val expected: String = Source.fromResource("omaha/results.txt").getLines().mkString("", "\n", "\n")


  "Submissions" should "be equals results" in {
    Console.withOut(out) {
      Console.withIn(in) {
        Runner.main(Array("--omaha"))
      }
    }

    out.toString mustBe expected
  }

  // 31.549 sec
  // 31.300 sec
  // 31.400 sec
  // 31.612 sec
  // 31.114 sec

  // Added parallelism for hands
  // 12.687 sec
  // 12.195 sec
  // 15.430 sec
  // 13.869 sec
  // 11.794 sec

}
