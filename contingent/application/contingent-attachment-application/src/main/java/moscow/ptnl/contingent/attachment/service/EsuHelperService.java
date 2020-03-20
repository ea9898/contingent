package moscow.ptnl.contingent.attachment.service;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.domain.esu.ESUEventHelper;
import moscow.ptnl.contingent.domain.esu.EsuEventBuilder;
import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import moscow.ptnl.contingent2.attachment.changearea.event.AttachOnAreaChange;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.messaging.MessageChannel;
import org.springframework.stereotype.Service;

import java.util.List;

import static moscow.ptnl.contingent.area.configuration.EventChannelsConfiguration.ESU_EVENT_CHANNEL_NAME;


@Service
public class EsuHelperService {
    
    private final static Logger LOG = LoggerFactory.getLogger(EsuHelperService.class);


    @Autowired @Qualifier(ESU_EVENT_CHANNEL_NAME)
    private MessageChannel esuChannel;

    /**
     * Отправляет событие в канал для отсылки в ЕСУ.
     * 
     * @param event не может быть null
     * @throws RuntimeException при невозможности определить имя топика на основе типа события
     */
    public void sendEventToESU(Object event) throws RuntimeException {
        
        LOG.debug("TRY SEND EVENT: {}", event);
        
        if (event == null) {
            throw new IllegalArgumentException("событие не может быть null");
        }
        
        esuChannel.send(EsuEventBuilder
                .withTopic(ESUEventHelper.resolveTopicName(event))
                .setEventObject(event)
                .buildMessage());
    }

}
