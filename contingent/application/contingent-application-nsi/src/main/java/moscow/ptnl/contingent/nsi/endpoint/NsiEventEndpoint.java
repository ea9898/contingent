package moscow.ptnl.contingent.nsi.endpoint;

import moscow.ptnl.contingent.nsi.domain.entity.NsiPushEvent;
import moscow.ptnl.contingent.nsi.repository.NsiPushEventCRUDRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.integration.annotation.MessageEndpoint;
import org.springframework.integration.annotation.ServiceActivator;
import org.springframework.messaging.Message;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.concurrent.Future;
import static moscow.ptnl.contingent.nsi.configuration.Constraint.NSI_EVENT_CHANNEL_NAME;

import moscow.ptnl.contingent.domain.Keyable;
import moscow.ptnl.contingent.nsi.domain.NsiPushEventConstraint;
import moscow.ptnl.util.ExceptionUtil;

/**
 * Точка получения событий из канала NSI_EVENT_CHANNEL_NAME.
 * 
 * @author m.kachalov
 */
@Service
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Throwable.class)
@MessageEndpoint
public class NsiEventEndpoint {

    private static final Logger LOG = LoggerFactory.getLogger(NsiEventEndpoint.class);  
    
    @Autowired
    private NsiEventProcessor eventProcessor;

    @Autowired
    private NsiPushEventCRUDRepository nsiPushEventCRUDRepository;

    @ServiceActivator(inputChannel = NSI_EVENT_CHANNEL_NAME)
    public void nsiPushConsumer(Message<Object> msg) {
        LOG.info("получено сообщение: " + msg);
        Keyable entity = (Keyable) msg.getPayload();
        String action = (String) msg.getHeaders().get(NsiPushEventConstraint.PUSH_EVENT_ACTION_HEADER);
        Long eventId = (Long) msg.getHeaders().get(NsiPushEventConstraint.PUSH_EVENT_ID_HEADER);
        NsiPushEvent event;

        try {
            event = nsiPushEventCRUDRepository.findById(eventId).get();
        } catch (Throwable error) {
            LOG.error("Не найдена запись о приеме сообщения с ID=" + eventId + ". Сообщение не обработано!", error);
            return;
        }
        try {
            Future<Void> result = eventProcessor.processMessage(entity, action);
            result.get(); //асинхронный вызов для создания разных транзакций
            event.setError(false);
        } catch (Throwable error) {
            event.setError(true);
            String errorMessage = ExceptionUtil.getStackTrace(error);
            event.setErrorMessage(errorMessage);
        }
    }
}
