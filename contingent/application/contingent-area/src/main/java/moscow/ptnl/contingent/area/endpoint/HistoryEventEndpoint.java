/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.contingent.area.endpoint;

import static moscow.ptnl.contingent.area.configuration.EventChannelsConfiguration.HISTORY_EVENT_CHANNEL_NAME;
import moscow.ptnl.contingent.domain.history.HistoryEvent;
import moscow.ptnl.contingent.repository.history.HistoryEventRepository;
import moscow.ptnl.ws.security.UserContextHolder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.integration.annotation.MessageEndpoint;
import org.springframework.integration.annotation.ServiceActivator;
import org.springframework.messaging.Message;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 * @author m.kachalov
 */
@Service
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Throwable.class)
@MessageEndpoint
public class HistoryEventEndpoint {
    
    private static final Logger LOG = LoggerFactory.getLogger(HistoryEventEndpoint.class);
    
    @Autowired
    private HistoryEventRepository historyEventRepository;
    
    /**
     * Получатель сообщений для записи в историю.
     * 
     * @param msg 
     */
    @ServiceActivator(inputChannel = HISTORY_EVENT_CHANNEL_NAME)
    public void historyEventConsumer(Message<HistoryEvent> msg) {
        LOG.debug("Received history event: {}", msg.getPayload().getObjectType());
        try {
            HistoryEvent event = msg.getPayload();
            historyEventRepository.save(event);
        } catch (Exception e) {
            LOG.error("ошибка записи события", e);
        }
    }
    
}
