package com.anddew.poker

import java.util.concurrent.{Executors, TimeUnit}

import cats.effect.{ExitCode, IO, IOApp, Resource, SyncIO}
import com.anddew.poker.model.Combination.TwoPair
import com.anddew.poker.model.{Card, Combination, Rank}
import com.anddew.poker.model.Rank._
import com.anddew.poker.model.Suit._
import cats.implicits._
import cats.instances.list.catsKernelStdOrderForList
import com.anddew.poker.Runner.handleSubmission
import scala.concurrent.duration._

import scala.concurrent.ExecutionContext

object Test extends IOApp.WithContext {

  override protected def executionContextResource: Resource[SyncIO, ExecutionContext] = {
    Resource.make(SyncIO(Executors.newFixedThreadPool(
      2
//      sys.runtime.availableProcessors()
    ))
    )(pool => SyncIO {
      pool.shutdown()
      pool.awaitTermination(10, TimeUnit.SECONDS)
    }).map(ExecutionContext.fromExecutorService)
  }

  def looped(count: Long = 0): IO[Unit] = for {
    _ <- IO.shift *> IO.sleep(3.seconds) *> IO(println(s"$count - ${Thread.currentThread().getName}"))
    list = List(1, 2, 3, 4, 5)
    _ <- looped(count + 1)
  } yield ()


  override def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- looped()
    } yield ExitCode.Success
  }

}
