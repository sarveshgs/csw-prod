package csw.services.csclient.cli

/**
 * Application object allowing program execution from command line.
 */
// $COVERAGE-OFF$
class CliApp(commandLineRunner: CommandLineRunner) {
  def start(args: Array[String]): Unit =
    ArgsParser.parse(args).foreach { options ⇒
      start(options)
    }

  def start(options: Options) = {
    options.op match {
      //adminApi
      case "create"             ⇒ commandLineRunner.create(options)
      case "update"             ⇒ commandLineRunner.update(options)
      case "get"                ⇒ commandLineRunner.get(options)
      case "delete"             ⇒ commandLineRunner.delete(options)
      case "list"               ⇒ commandLineRunner.list(options)
      case "history"            ⇒ commandLineRunner.history(options)
      case "historyActive"      ⇒ commandLineRunner.historyActive(options)
      case "setActiveVersion"   ⇒ commandLineRunner.setActiveVersion(options)
      case "resetActiveVersion" ⇒ commandLineRunner.resetActiveVersion(options)
      case "getActiveVersion"   ⇒ commandLineRunner.getActiveVersion(options)
      case "getActiveByTime"    ⇒ commandLineRunner.getActiveByTime(options)
      case "getMetadata"        ⇒ commandLineRunner.getMetadata(options)
      //clientApi
      case "exists"    ⇒ commandLineRunner.exists(options)
      case "getActive" ⇒ commandLineRunner.getActive(options)
      case x           ⇒ throw new RuntimeException(s"Unknown operation: $x")
    }
  }
}
// $COVERAGE-ON$
