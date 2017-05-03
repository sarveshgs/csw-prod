package csw.services.location.commons

import akka.testkit.TestActorRef
import csw.services.location.commons.TestFutureExtension.RichFuture
import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

class BlockingUtilsTest extends FunSuite with Matchers {
  test("test that Poll method bottoms out and returns expected result") {
    BlockingUtils.poll(true) shouldBe true
    BlockingUtils.poll(false, 1.seconds) shouldBe false
  }

  test("test that Poll method detects predicate fulfillment") {

    import scala.concurrent.ExecutionContext.Implicits.global

    val upMembers                  = 10
    val replicaCountF: Future[Int] = Future { Thread.sleep(2000); 10 }
    def replicaCount: Int =
      if (replicaCountF.isCompleted) replicaCountF.value match {
        case Some(Success(value)) ⇒ value
        case _                    ⇒ -1
      } else -1

    def predicate = replicaCount == upMembers

    val result = Future {
      BlockingUtils.poll(predicate, 10.seconds)
    }.await

    result shouldBe true
  }
}
