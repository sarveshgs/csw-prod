package csw.services.location.models;

import akka.actor.ActorRef;
import csw.services.location.models.Connection.*;

import java.net.URI;
import java.util.Optional;
import scala.compat.java8.OptionConverters;

public class JLocation {

    public static ResolvedAkkaLocation resolvedAkkaLocation(Connection.AkkaConnection akkaConnection, URI uri, String prefix, Optional<ActorRef> actorRef) {
        return new ResolvedAkkaLocation(akkaConnection, uri, prefix, OptionConverters.toScala(actorRef));
    }

    public static ResolvedHttpLocation resolvedHttpLocation(HttpConnection httpConnection, URI uri, String path) {
        return new ResolvedHttpLocation(httpConnection, uri, path);
    }

    public static ResolvedTcpLocation resolvedTcpLocation(TcpConnection tcpConnection, URI uri) {
        return new ResolvedTcpLocation(tcpConnection, uri);
    }
}