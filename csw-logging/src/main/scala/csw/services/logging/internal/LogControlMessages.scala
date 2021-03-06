package csw.services.logging.internal

import csw.services.logging.internal.LoggingLevels.Level

// Parent trait for Messages which will be send to components for interacting with its logging system
sealed trait LogControlMessages

// Message to get Logging configuration metadata of the receiver
case class GetComponentLogMetadata(componentName: String) extends LogControlMessages

// Message to change the log level of any component
case class SetComponentLogLevel(componentName: String, logLevel: Level) extends LogControlMessages
