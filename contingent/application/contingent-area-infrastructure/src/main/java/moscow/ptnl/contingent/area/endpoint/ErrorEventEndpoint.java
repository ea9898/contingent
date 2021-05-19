package moscow.ptnl.contingent.area.endpoint;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.integration.annotation.MessageEndpoint;
import org.springframework.integration.annotation.ServiceActivator;
import org.springframework.integration.context.IntegrationContextUtils;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.ErrorMessage;
import org.springframework.stereotype.Component;

/**
 * Логирование ошибок попавших в канал для ошибок.
 * 
 * @author m.kachalov
 */
@Component
@MessageEndpoint
public class ErrorEventEndpoint {
    
    private final static Logger LOG = LoggerFactory.getLogger(ErrorEventEndpoint.class);
    
    @ServiceActivator(inputChannel = IntegrationContextUtils.ERROR_CHANNEL_BEAN_NAME)
    public void logErrorEvent(Message<?> msg) {
        try {
            ErrorMessage em = (ErrorMessage) msg;
            String errorMessage = em.getPayload().getMessage();
            LOG.error("Ошибка обработки сообщения в SI", errorMessage);
        } catch (Exception e) {
            LOG.error("Ошибка обработки сообщения об ошибке в SI", e);
        }
    }    
    
}
