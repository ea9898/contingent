package moscow.ptnl.contingent.nsi.endpoint;

import moscow.ptnl.contingent.domain.nsi.entity.NsiPushEvent;
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
import moscow.ptnl.contingent.domain.nsi.NsiPushEventConstraint;
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
        try {
            Future<Void> result = eventProcessor.processMesage(entity, action);        
            result.get(); //асинхронный вызов для создания разных транзакций
        } catch (Throwable error) {
            saveError(error, msg);
        }
    }
    
    private void saveError(Throwable error, Message<Object> msg) {
        try {
            NsiPushEvent event = nsiPushEventCRUDRepository.findById((Long) msg.getHeaders().get(NsiPushEventConstraint.PUSH_EVENT_ID_HEADER)).get();
            event.setError(true);
            String errorMessage = ExceptionUtil.getStackTrace(error);
            event.setErrorMessage(errorMessage);            
            nsiPushEventCRUDRepository.save(event);
        } catch (Throwable e) {
            LOG.error("ошибка сохранения сведений об ошибке", e);
        }
    }
    
    
}
