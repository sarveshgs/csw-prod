package csw.services.location

import csw.services.location.common.ActorRuntime
import csw.services.location.common.TestFutureExtension.RichFuture
import csw.services.location.models.{Location, ResolvedAkkaLocation}
import csw.services.location.scaladsl.LocationService
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}

class LocationServiceIntegrationTest
    extends FunSuite
    with Matchers
    with MockFactory
    with BeforeAndAfter {

  private val actorRuntime = new ActorRuntime("AssemblySystem")
  private val locationService = LocationService.make(actorRuntime)

  test("resolves remote HCD") {

    val listOfLocations = locationService.list.await
    val hcdLocation: Location = listOfLocations(0)

    listOfLocations should not be empty
    listOfLocations should have size 1
    hcdLocation shouldBe a[ResolvedAkkaLocation]
    hcdLocation
      .asInstanceOf[ResolvedAkkaLocation]
      .uri
      .toString should not be empty
  }
}