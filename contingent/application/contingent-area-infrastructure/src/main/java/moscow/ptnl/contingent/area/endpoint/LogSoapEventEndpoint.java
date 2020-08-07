package moscow.ptnl.contingent.area.endpoint;

import moscow.ptnl.contingent.domain.history.HistoryRequest;
import moscow.ptnl.contingent.repository.history.HistoryRequestsRepository;
import moscow.ptnl.contingent.util.AreaServiceLogMethodsEnum;
import moscow.ptnl.soap.log.SoapContextData;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.integration.annotation.MessageEndpoint;
import org.springframework.integration.annotation.ServiceActivator;
import org.springframework.messaging.Message;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import static moscow.ptnl.contingent.area.configuration.EventChannelsConfiguration.SOAP_LOG_EVENT_CHANNEL_NAME;
import org.springframework.stereotype.Component;

/**
 *
 * @author sorlov
 */
@Component
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Throwable.class)
@MessageEndpoint
public class LogSoapEventEndpoint {
    
    private static final Logger LOG = LoggerFactory.getLogger(LogSoapEventEndpoint.class);
    
    @Autowired
    private HistoryRequestsRepository historyRequestsRepository;
    
    /**
     * Получатель сообщений для записи SOAP лога.
     * 
     * @param msg 
     */
    @ServiceActivator(inputChannel = SOAP_LOG_EVENT_CHANNEL_NAME)
    public void logSoapEventConsumer(Message<SoapContextData> msg) {
        LOG.trace("Received SOAP event: {}", msg.getPayload().getMethod());

        try {
            String[] nameParts = msg.getPayload().getMethod().split("/");
            if (nameParts.length > 1 && AreaServiceLogMethodsEnum.contains(nameParts[1])) {
                HistoryRequest event = HistoryRequest.build(msg.getPayload());
                historyRequestsRepository.save(event);
            }
        } catch (Exception e) {
            LOG.error("Can't write SOAP log event", e);
        }
    }
}
