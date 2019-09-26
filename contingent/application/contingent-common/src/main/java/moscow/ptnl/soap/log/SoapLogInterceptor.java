package moscow.ptnl.soap.log;

import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.io.CachedOutputStream;
import org.apache.cxf.message.Message;
import org.apache.cxf.message.MessageUtils;
import org.apache.cxf.phase.AbstractPhaseInterceptor;
import org.apache.cxf.phase.Phase;
import org.apache.cxf.service.model.OperationInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.integration.support.MessageBuilder;
import org.springframework.messaging.MessageChannel;
import org.springframework.stereotype.Component;

import javax.xml.stream.XMLStreamWriter;
import java.io.OutputStream;
import java.time.LocalDateTime;
import java.util.concurrent.Callable;

@Component
public class SoapLogInterceptor extends AbstractPhaseInterceptor<Message> {

    private static final Logger LOG = LoggerFactory.getLogger(SoapLogInterceptor.class);

    private MessageChannel eventChannel;
    private Callable<String> uuidProvider;

    SoapLogInterceptor() {
        super(Phase.SEND);
    }

    @Override
    public void handleMessage(Message message) throws Fault {
        try {
            if (MessageUtils.isRequestor(message) ^ MessageUtils.isOutbound(message) && message.getExchange() != null) {
                SoapContextData data = new SoapContextData();

                try (CachedOutputStream contentIn = message.getExchange().getInMessage().getContent(CachedOutputStream.class)) {
                    CachedOutputStream contentOut = (CachedOutputStream) message.getContent(OutputStream.class);
                    StringBuilder name = new StringBuilder();
                    OperationInfo opInfo = message.getExchange().get(OperationInfo.class);
                    name.append(message.getExchange().getService().getName().getLocalPart())
                            .append("/")
                            .append(opInfo == null ? "[Null]" : opInfo.getName().getLocalPart());
                    message.getContent(XMLStreamWriter.class).flush();
                    data.setUuid(uuidProvider == null ? null : uuidProvider.call());
                    data.setRequest(new String(contentIn.getBytes()));
                    data.setResponse(new String(contentOut.getBytes()));
                    data.setMethod(name.toString());
                    data.setTime(LocalDateTime.now());
                }
                tryToSendEvent(data);
            }
        }
        catch(Throwable th){
            LOG.error("Can't log SOAP message", th);
        }
    }

    private void tryToSendEvent(SoapContextData data) {
        if (eventChannel == null || data == null) {
            return;
        }
        eventChannel.send(MessageBuilder
                .withPayload(data)
                .build());
    }

    public MessageChannel getEventChannel() {
        return eventChannel;
    }

    public void setEventChannel(MessageChannel eventChannel) {
        this.eventChannel = eventChannel;
    }

    public Callable<String> getUuidProvider() {
        return uuidProvider;
    }

    public void setUuidProvider(Callable<String> uuidProvider) {
        this.uuidProvider = uuidProvider;
    }
}
