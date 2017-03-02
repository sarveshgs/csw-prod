package csw.services.location.common

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import com.typesafe.config.ConfigFactory

import scala.concurrent.ExecutionContext
import scala.collection.JavaConverters._

class ActorRuntime(name: String, port: Int = 2552) {

  private def config = {
    val configs = Map("akka.remote.netty.tcp.hostname" -> Networks.getPrimaryIpv4Address.getHostAddress,
      "akka.remote.netty.tcp.port" -> port).asJava

    ConfigFactory.parseMap(configs)
      .withFallback(ConfigFactory.load())
  }

  implicit val actorSystem: ActorSystem = ActorSystem(name, config)
  implicit val ec: ExecutionContext = actorSystem.dispatcher
  implicit val mat: Materializer = makeMat()

  def makeMat(): Materializer = ActorMaterializer()
}