package csw.apps.clusterseed.admin.http

import akka.Done
import akka.http.scaladsl.server.Route
import csw.apps.clusterseed.admin.LogAdmin
import csw.apps.clusterseed.admin.internal.ActorRuntime

class AdminRoutes(adminExceptionHandler: AdminExceptionHandler, logAdmin: LogAdmin, actorRuntime: ActorRuntime)
    extends HttpSupport {

  import actorRuntime._
  val route: Route = routeLogger {
    handleExceptions(adminExceptionHandler.exceptionHandler) {
      path("admin" / "logging" / Segment / "level") { componentName ⇒
        get {
          complete(logAdmin.getLogMetadata(componentName))
        } ~
        post {
          logLevelParam { (logLevel) ⇒
            complete(logAdmin.setLogLevel(componentName, logLevel).map(_ ⇒ Done))
          }
        }
      }
    }
  }

}
