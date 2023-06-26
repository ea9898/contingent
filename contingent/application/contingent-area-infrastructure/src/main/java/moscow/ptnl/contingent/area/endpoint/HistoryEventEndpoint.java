package moscow.ptnl.contingent.area.endpoint;

import static moscow.ptnl.contingent.area.configuration.EventChannelsConfiguration.HISTORY_EVENT_CHANNEL_NAME;
import moscow.ptnl.contingent.domain.history.HistoryEvent;
import moscow.ptnl.contingent.nsi.domain.area.EventTypes;
import moscow.ptnl.contingent.repository.history.EventTypesCRUDRepository;
import moscow.ptnl.contingent.repository.history.HistoryEventCRUDRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.integration.annotation.MessageEndpoint;
import org.springframework.integration.annotation.ServiceActivator;
import org.springframework.messaging.Message;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 * @author m.kachalov
 */
@Component
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Throwable.class)
@MessageEndpoint
public class HistoryEventEndpoint {
    
    private static final Logger LOG = LoggerFactory.getLogger(HistoryEventEndpoint.class);
    
    @Autowired
    private HistoryEventCRUDRepository historyEventCRUDRepository;

    @Autowired
    private EventTypesCRUDRepository eventTypesCRUDRepository;

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

            if (event.getEventType() != null && event.getEventType().getId() != null) {
                Long eventTypeId = event.getEventType().getId();
                EventTypes eventType = eventTypesCRUDRepository.findById(eventTypeId).orElse(null);
                event.setEventType(eventType);

                if (eventType == null) {
                    LOG.warn("Не найдена запись в справочнике EVENT_TYPES с ИД={}", eventTypeId);
                }
            }
            historyEventCRUDRepository.save(event);
        } catch (Exception e) {
            LOG.error("ошибка записи события", e);
        }
    }
    
}
