package csw.services.location.javadsl

import java.util
import java.util.concurrent.CompletionStage

import akka.Done
import akka.stream.KillSwitch
import akka.stream.javadsl.Source
import csw.services.location.common.ActorRuntime
import csw.services.location.models._
import csw.services.location.scaladsl.LocationService

import scala.collection.JavaConverters._
import scala.compat.java8.FutureConverters._

private class JLocationServiceImpl(locationService: LocationService, actorRuntime: ActorRuntime) extends ILocationService{

  import actorRuntime._

  override def register(registration: Registration): CompletionStage[RegistrationResult] =
    locationService.register(registration).toJava

  override def unregister(connection: Connection): CompletionStage[Done] =
    locationService.unregister(connection).toJava

  override def unregisterAll(): CompletionStage[Done] =
    locationService.unregisterAll().toJava

  override def resolve(connection: Connection): CompletionStage[Resolved] =
    locationService.resolve(connection).toJava

  override def resolve(connections: util.Set[Connection]): CompletionStage[util.Set[Resolved]] =
    locationService.resolve(connections.asScala.toSet).map(_.asJava).toJava

  override def list(): CompletionStage[util.List[Location]] =
    locationService.list.map(_.asJava).toJava

  override def list(componentType: ComponentType): CompletionStage[util.List[Location]] =
    locationService.list(componentType).map(_.asJava).toJava

  override def list(hostname: String): CompletionStage[util.List[Resolved]] =
    locationService.list(hostname).map(_.asJava).toJava

  override def list(connectionType: ConnectionType): CompletionStage[util.List[Location]] =
    locationService.list(connectionType).map(_.asJava).toJava

  override def track(connection: Connection): Source[Location, KillSwitch] =
    locationService.track(connection).asJava
}