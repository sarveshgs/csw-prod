package csw.services.config.models

import java.time.Instant

/**
 * Holds information about a specific version of a config file
 */
case class ConfigFileHistory(id: ConfigId, comment: String, time: Instant)