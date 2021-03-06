package csw.services.location

import akka.actor.Props
import akka.testkit.TestProbe
import csw.services.location.commons.CswCluster
import csw.services.location.helpers.{LSNodeSpec, TwoMembersAndSeed}
import csw.services.location.models.Connection.AkkaConnection
import csw.services.location.models._
import csw.services.location.scaladsl.LocationServiceFactory
import org.jboss.netty.logging.{InternalLoggerFactory, Slf4JLoggerFactory}

import scala.concurrent.Await
import scala.concurrent.duration._

class DetectComponentRestartTestMultiJvmNode1 extends DetectComponentRestartTest(0)
class DetectComponentRestartTestMultiJvmNode2 extends DetectComponentRestartTest(0)
class DetectComponentRestartTestMultiJvmNode3 extends DetectComponentRestartTest(0)

class DetectComponentRestartTest(ignore: Int) extends LSNodeSpec(config = new TwoMembersAndSeed) {

  import config._

  // Fix to avoid 'java.util.concurrent.RejectedExecutionException: Worker has already been shutdown'
  InternalLoggerFactory.setDefaultFactory(new Slf4JLoggerFactory)

  test("should detect re-registering of new location for a connection that has crashed/gone away") {

    val akkaConnection = AkkaConnection(ComponentId("TromboneHcd", ComponentType.HCD))

    runOn(member1) {
      locationService.register(AkkaRegistration(akkaConnection, system.actorOf(Props.empty))).await
      enterBarrier("location-registered")
      enterBarrier("location-updated")

      Await.ready(system.whenTerminated, 10.seconds)

      val newSystem = startNewSystem()

      enterBarrier("location-removed")
      val freshLocationService = LocationServiceFactory.withCluster(CswCluster.withSystem(newSystem))
      Thread.sleep(2000)

      freshLocationService.register(AkkaRegistration(akkaConnection, newSystem.actorOf(Props.empty))).await
      enterBarrier("member-re-registered")
    }

    runOn(seed, member2) {
      enterBarrier("location-registered")
      val testProbe = TestProbe()
      locationService.subscribe(akkaConnection, testProbe.testActor ! _)

      testProbe.expectMsgType[LocationUpdated]
      enterBarrier("location-updated")

      runOn(seed) {
        Await.result(testConductor.shutdown(member1), 10.seconds)
      }

      testProbe.expectMsgType[LocationRemoved](5.seconds)
      enterBarrier("location-removed")

      enterBarrier("member-re-registered")
      testProbe.expectMsgType[LocationUpdated](5.seconds)
    }

    enterBarrier("after-2")
  }

}
