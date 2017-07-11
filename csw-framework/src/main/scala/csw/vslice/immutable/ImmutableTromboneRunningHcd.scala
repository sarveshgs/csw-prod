package csw.vslice.immutable

import akka.typed.scaladsl.Actor
import akka.typed.{ActorRef, Behavior}
import csw.vslice.hcd.models.AxisRequest.{GetStatistics, PublishAxisUpdate}
import csw.vslice.hcd.models.AxisResponse._
import csw.vslice.framework.FromComponentLifecycleMessage.ShutdownComplete
import csw.vslice.framework.ToComponentLifecycleMessage._
import csw.vslice.hcd.models.{AxisConfig, AxisRequest, AxisResponse}
import csw.param.UnitsOfMeasure.encoder
import csw.vslice.framework.ToComponentLifecycleMessage
import csw.vslice.immutable.TromboneHcdMessage._

sealed trait TromboneHcdMessage

object TromboneHcdMessage {

  sealed trait TromboneEngineering                             extends TromboneHcdMessage
  case object GetAxisStats                                     extends TromboneEngineering
  case object GetAxisUpdate                                    extends TromboneEngineering
  case class GetAxisUpdateNow(replyTo: ActorRef[AxisResponse]) extends TromboneEngineering
  case object GetAxisConfig                                    extends TromboneEngineering

  case class Lifecycle(message: ToComponentLifecycleMessage) extends TromboneHcdMessage
  case class AxisResponseE(message: AxisResponse)            extends TromboneHcdMessage
}

case class TromboneState(current: AxisUpdate,
                         stats: AxisStatistics,
                         axisConfig: AxisConfig,
                         tromboneAxis: ActorRef[AxisRequest])

object TromboneRunningHcd extends HcdRunningBehavior[TromboneHcdMessage, TromboneState] {
  override def run(state: TromboneState): Behavior[TromboneHcdMessage] = Actor.immutable { (ctx, msg) ⇒
    def dd(message: TromboneEngineering): Behavior[TromboneHcdMessage] = message match {
      case GetAxisStats =>
        val wrapper = ctx.spawnAdapter { x: AxisResponse ⇒
          AxisResponseE(x)
        }
        state.tromboneAxis ! GetStatistics(wrapper); Actor.same
      case GetAxisUpdate             => state.tromboneAxis ! PublishAxisUpdate; Actor.same
      case GetAxisUpdateNow(replyTo) => replyTo ! state.current; Actor.same
      case GetAxisConfig =>
        import csw.vslice.hcd.models.TromboneHcdState._
        val axisConfigState = defaultConfigState.madd(
          lowLimitKey    -> state.axisConfig.lowLimit,
          lowUserKey     -> state.axisConfig.lowUser,
          highUserKey    -> state.axisConfig.highUser,
          highLimitKey   -> state.axisConfig.highLimit,
          homeValueKey   -> state.axisConfig.home,
          startValueKey  -> state.axisConfig.startPosition,
          stepDelayMSKey -> state.axisConfig.stepDelayMS
        )
        //TODO: PubSub
        Actor.same

    }

    def ee(message: ToComponentLifecycleMessage): Behavior[TromboneHcdMessage] = message match {
      case DoShutdown     => println("Received doshutdown"); Actor.same
      case DoRestart      => println("Received dorestart"); Actor.same
      case Running        => println("Received running"); Actor.same
      case RunningOffline => println("Received running offline"); Actor.same
      case LifecycleFailureInfo(state1, reason) =>
        println(s"Received failed state: $state for reason: $reason"); Actor.same
      case ShutdownComplete ⇒
        println("shutdown complete")
        Actor.same
    }

    def ff(message: AxisResponse): Behavior[TromboneHcdMessage] = message match {
      case AxisStarted => Actor.same
      case AxisFinished(newRef) =>
        val newState = state.copy(tromboneAxis = newRef)
        run(newState)
      case au @ AxisUpdate(axisName, axisState, current, inLowLimit, inHighLimit, inHomed) =>
        import csw.vslice.hcd.models.TromboneHcdState._
        val tromboneAxisState = defaultAxisState.madd(
          positionKey    -> current withUnits encoder,
          stateKey       -> axisState.toString,
          inLowLimitKey  -> inLowLimit,
          inHighLimitKey -> inHighLimit,
          inHomeKey      -> inHomed
        )
        //TODO: PubSub
        run(state.copy(current = au))
      case AxisFailure(reason) => Actor.same
      case as: AxisStatistics =>
        import csw.vslice.hcd.models.TromboneHcdState._
        val tromboneStats = defaultStatsState.madd(
          datumCountKey   -> as.initCount,
          moveCountKey    -> as.moveCount,
          limitCountKey   -> as.limitCount,
          homeCountKey    -> as.homeCount,
          successCountKey -> as.successCount,
          failureCountKey -> as.failureCount,
          cancelCountKey  -> as.cancelCount
        )
        //TODO: PubSub
        run(state.copy(stats = as))
    }

    msg match {
      case message: TromboneEngineering => dd(message)
      case Lifecycle(message)           => ee(message)
      case AxisResponseE(axisResponse)  => ff(axisResponse)
    }
  }
}