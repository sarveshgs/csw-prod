package csw.services.config.commons

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}

import scala.concurrent.ExecutionContextExecutor

class ActorRuntime(_actorSystem: ActorSystem) {
  implicit val actorSystem: ActorSystem = _actorSystem
  implicit val ex: ExecutionContextExecutor = actorSystem.dispatcher
  implicit val mat: Materializer = ActorMaterializer()
}