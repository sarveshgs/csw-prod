package csw.services.location.models;

/**
 * Java API for location service connection type: Indicates if it is an http server or an akka actor.
 */
@SuppressWarnings("unused")
public class JConnectionType {
    /**
     * Connection type of an Akka actor based service
     */
    public static final ConnectionType AkkaType = ConnectionType.AkkaType$.MODULE$;

    /**
     * Connection type of an TCP based service
     */
    public static final ConnectionType TcpType = ConnectionType.TcpType$.MODULE$;

    /**
     * Connection type of a HTTP based service
     */
    public static final ConnectionType HttpType = ConnectionType.HttpType$.MODULE$;
}