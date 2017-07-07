package csw.services.logging.scaladsl

import akka.actor.ActorRef
import com.persist.JsonOps.JsonObject
import csw.services.logging.components.IrisSupervisorActor._
import csw.services.logging.components._
import csw.services.logging.internal.LoggingLevels._
import csw.services.logging.utils.LoggingTestSuite

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class LoggerCompTest extends LoggingTestSuite {

  private val irisSupervisorActorRef = actorSystem.actorOf(IrisSupervisorActor.props())
  private val irisUtilActorRef       = actorSystem.actorOf(IrisActorUtil.props)
  private val irisTLA                = new IrisTLA()
  private val irisUtil               = new IrisUtil()
  private val tromboneHcd            = new TromboneHcd()

  private var componentLogBuffer: mutable.Map[String, ArrayBuffer[JsonObject]] = mutable.Map.empty
  var genericLogBuffer                                                         = mutable.Buffer.empty[JsonObject]
  private var irisLogBuffer                                                    = mutable.Buffer.empty[JsonObject]
  private var tromboneHcdLogBuffer                                             = mutable.Buffer.empty[JsonObject]

  def sendMessagesToActor(actorRef: ActorRef) = {
    actorRef ! LogTrace
    actorRef ! LogDebug
    actorRef ! LogInfo
    actorRef ! LogWarn
    actorRef ! LogError
    actorRef ! LogFatal
  }

  def allComponentsStartLogging() = {
    //componentName = IRIS
    sendMessagesToActor(irisSupervisorActorRef)
    irisTLA.startLogging(logMsgMap)
    //Generic Logger
    sendMessagesToActor(irisUtilActorRef)
    irisUtil.startLogging(logMsgMap)
    //componentName = tromboneHcd
    tromboneHcd.startLogging(logMsgMap)
    Thread.sleep(200)
  }

  def splitAndGroupLogs() = {
    // clear all logs
    componentLogBuffer = mutable.Map.empty
    irisLogBuffer.clear()
    genericLogBuffer.clear()
    tromboneHcdLogBuffer.clear()

    logBuffer.foreach { log ⇒
      log.get("@componentName") match {
        case Some(_) ⇒
          val name = log("@componentName").toString
          componentLogBuffer.get(name) match {
            case Some(xs) ⇒ componentLogBuffer.update(name, xs :+ log)
            case None     ⇒ componentLogBuffer.put(name, ArrayBuffer(log))
          }
        case None ⇒ genericLogBuffer += log
      }
    }
    irisLogBuffer = componentLogBuffer(IrisSupervisorActor.NAME)
    tromboneHcdLogBuffer = componentLogBuffer(TromboneHcd.NAME)

    logBuffer.clear()
  }

  // This test simulates single jvm multiple components use cases
  test("changing log level of component should only affect component specific classes") {
    allComponentsStartLogging

    // extract component and non-component logs and group them
    splitAndGroupLogs

    def testLogBuffer(logBuffer: mutable.Buffer[JsonObject], configuredLogLevel: Level): Unit = {
      logBuffer.foreach { log ⇒
        val currentLogLevel = log("@severity").toString.toLowerCase
        Level(currentLogLevel) >= configuredLogLevel shouldBe true
      }
    }

    irisLogBuffer.size shouldBe 4
    testLogBuffer(irisLogBuffer, ERROR)

    genericLogBuffer.size shouldBe 12
    testLogBuffer(genericLogBuffer, TRACE)

    tromboneHcdLogBuffer.size shouldBe 5
    testLogBuffer(tromboneHcdLogBuffer, DEBUG)

    // setting log level of IRIS comp to FATAL and it should not change log levels of other comps or generic classes
    loggingSystem.setComponentLogLevel(IrisSupervisorActor.NAME, FATAL)

    // start logging at all component levels
    allComponentsStartLogging

    // extract component and non-component logs and group them
    splitAndGroupLogs

    irisLogBuffer.size shouldBe 2
    testLogBuffer(irisLogBuffer, FATAL)

    genericLogBuffer.size shouldBe 12
    testLogBuffer(genericLogBuffer, TRACE)

    tromboneHcdLogBuffer.size shouldBe 5
    testLogBuffer(tromboneHcdLogBuffer, DEBUG)
  }
}