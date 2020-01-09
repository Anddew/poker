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

}
