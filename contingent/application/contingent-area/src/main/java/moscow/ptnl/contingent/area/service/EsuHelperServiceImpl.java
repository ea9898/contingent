package moscow.ptnl.contingent.area.service;

import java.util.List;
import static moscow.ptnl.contingent.area.configuration.EventChannelsConfiguration.ESU_EVENT_CHANNEL_NAME;

import moscow.ptnl.contingent.transform.model.esu.AreaInfoEventMapper;
import moscow.ptnl.contingent.transform.model.esu.AttachOnAreaChangeMapper;
import moscow.ptnl.contingent.domain.area.Algorithms;
import moscow.ptnl.contingent.domain.area.EsuHelperService;
import moscow.ptnl.contingent.domain.esu.EsuEventBuilder;
import moscow.ptnl.contingent.domain.esu.ESUEventHelper;
import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import moscow.ptnl.contingent2.attachment.changearea.event.AttachOnAreaChange;
import moscow.ptnl.contingent.domain.area.entity.Area;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.messaging.MessageChannel;
import org.springframework.stereotype.Service;


@Service
public class EsuHelperServiceImpl implements EsuHelperService {
    
    private final static Logger LOG = LoggerFactory.getLogger(EsuHelperServiceImpl.class);

    @Autowired
    private Algorithms algorithms;

    @Autowired @Qualifier(ESU_EVENT_CHANNEL_NAME)
    private MessageChannel esuChannel;

    @Autowired
    private AttachOnAreaChangeMapper attachOnAreaChangeMapper;

    @Autowired
    private AreaInfoEventMapper areaInfoEventMapper;

    /**
     * Создает объект топика {@link moscow.ptnl.contingent2.area.info.AreaInfoEvent} 
     * и отправляет его в канал для отсылки в ЕСУ.
     * 
     * @param area 
     * @param methodName 
     */
    @Override
    public void sendAreaInfoEvent(Area area, String methodName) {
        AreaInfoEvent event = createAreaInfoEvent(area, methodName);
        sendEventToESU(event);
    }

    /**
     * Создает объект топика {@link moscow.ptnl.contingent2.attachment.changearea.event.AttachOnAreaChange} 
     * и отправляет его в канал для отсылки в ЕСУ.
     * 
     * @param primaryAreasIdCreateAttachments
     * @param primaryAreasIdCloseAttachments
     * @param dependentArea 
     */
    @Override
    public void sendAttachOnAreaChangeEvent(
            List<Long> primaryAreasIdCreateAttachments,
            List<Long> primaryAreasIdCloseAttachments,
            Area dependentArea) {
        AttachOnAreaChange event = createTopicCreateCloseAttachAreaChange(
            primaryAreasIdCreateAttachments,
            primaryAreasIdCloseAttachments,
            dependentArea);
        sendEventToESU(event);
    }
    
    /**
     * Отправляет событие в канал для отсылки в ЕСУ.
     * 
     * @param event не может быть null
     * @throws RuntimeException при невозможности определить имя топика на основе типа события
     */
    private void sendEventToESU(Object event) throws RuntimeException {
        
        LOG.debug("TRY SEND EVENT: {}", event);
        
        if (event == null) {
            throw new IllegalArgumentException("событие не может быть null");
        }
        
        esuChannel.send(EsuEventBuilder
                .withTopic(ESUEventHelper.resolveTopicName(event))
                .setEventObject(event)
                .buildMessage());
    }
    
    private AreaInfoEvent createAreaInfoEvent(Area area, String methodName) {
        return areaInfoEventMapper.entityToDtoTransform(algorithms.createTopicAreaInfo(area, methodName));
    }
    
    private AttachOnAreaChange createTopicCreateCloseAttachAreaChange(
            List<Long> primaryAreasIdCreateAttachments,
            List<Long> primaryAreasIdCloseAttachments,
            Area dependentArea) {
        return attachOnAreaChangeMapper.entityToDtoTransform(algorithms.createTopicCreateCloseAttachAreaChange(
                primaryAreasIdCreateAttachments,
                primaryAreasIdCloseAttachments,
                dependentArea
        ));
    }
    
    

}
