package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.service.esu.EsuService;
import moscow.ptnl.contingent.area.repository.area.AreaRepository;
import moscow.ptnl.contingent.configuration.EventChannelsConfiguration;
import moscow.ptnl.contingent.domain.esu.EsuEventBuilder;
import moscow.ptnl.contingent.domain.esu.event.ESUEventHelper;
import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import moscow.ptnl.contingent2.attachment.changearea.event.AttachOnAreaChange;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.messaging.MessageChannel;
import org.springframework.stereotype.Service;


@Service
public class EsuHelperService {
    
    private final static Logger LOG = LoggerFactory.getLogger(EsuHelperService.class);

    @Autowired
    private AreaRepository areaRepository;

    @Autowired
    private EsuService esuService;

    @Autowired
    private Algorithms algorithms;

    @Autowired @Qualifier(EventChannelsConfiguration.ESU_EVENT_CHANNEL_NAME)
    private MessageChannel esuChannel;

    /**
     * Метод принимающий объект топика AreaInfoEvent и отправляет в канал для отправки в ЕСУ.
     * @param event 
     */
    public void sendAreaInfoEventTopicToESU(AreaInfoEvent event) {
        esuChannel.send(EsuEventBuilder
                .withTopic(ESUEventHelper.resolveTopicName(event))
                .setEventObject(event)
                .buildMessage());
    }

    public void sendAttachOnAreaChangeTopicToEsu(AttachOnAreaChange event) {
        esuChannel.send(EsuEventBuilder
                .withTopic(ESUEventHelper.resolveTopicName(event))
                .setEventObject(event)
                .buildMessage());
    }
    
    /**
     * Отправляет событие в канал для отправки в ЕСУ.
     * 
     * @param event не может быть null
     * @throws RuntimeException при невозможности определить имя топика на основе типа события
     */
    public void sendEventToESU(Object event) throws RuntimeException {
        LOG.info("TRY SEND EVENT: {}", event);
        esuChannel.send(EsuEventBuilder
                .withTopic(ESUEventHelper.resolveTopicName(event))
                .setEventObject(event)
                .buildMessage());
    }
    
    
    
    

}
