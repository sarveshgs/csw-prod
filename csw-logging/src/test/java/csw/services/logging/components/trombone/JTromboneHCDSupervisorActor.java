package csw.services.logging.components.trombone;

import csw.services.logging.javadsl.ILogger;

public class JTromboneHCDSupervisorActor extends JTromboneHCDActorLogger{

    private ILogger log = getLogger();

    @Override
    public Receive createReceive() {
        return receiveBuilder()
                .match(String.class, msg -> msg.equals("trace"), msg -> log.trace(() -> msg))
                .match(String.class, msg -> msg.equals("debug"), msg -> log.debug(() -> msg))
                .match(String.class, msg -> msg.equals("info"), msg -> log.info(() -> msg))
                .match(String.class, msg -> msg.equals("warn"), msg -> log.warn(() -> msg))
                .match(String.class, msg -> msg.equals("error"), msg -> log.error(() -> msg))
                .match(String.class, msg -> msg.equals("fatal"), msg -> log.fatal(() -> msg))
                .build();
    }
}
