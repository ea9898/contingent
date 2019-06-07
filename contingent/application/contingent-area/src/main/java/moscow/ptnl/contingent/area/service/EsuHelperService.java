package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.service.esu.EsuService;
import moscow.ptnl.contingent.area.repository.area.AreaRepository;
import moscow.ptnl.contingent.configuration.EventChannelsConfiguration;
import moscow.ptnl.contingent.domain.esu.EsuEventBuilder;
import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import moscow.ptnl.contingent2.attachment.changearea.event.AttachOnAreaChange;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.messaging.MessageChannel;
import org.springframework.stereotype.Component;


@Component
public class EsuHelperService {

    @Autowired
    private AreaRepository areaRepository;

    @Autowired
    private EsuService esuService;

    @Autowired
    private Algorithms algorithms;

    @Autowired @Qualifier(EventChannelsConfiguration.ESU_EVENT_CHANNEL_NAME)
    private MessageChannel esuChannel;

    // Метод принимающий объект топика AreaInfoEvent и отправляет в канал для отправки в ЕСУ
    public void sendAreaInfoEventTopicToESU(AreaInfoEvent areaInfoEvent) {
        esuChannel.send(EsuEventBuilder
                .withTopic("")
                .setMessage(areaInfoEvent)
                .buildMessage());
            }

    public void sendAttachOnAreaChangeTopicToEsu(AttachOnAreaChange attachOnAreaChange) {
        esuChannel.send(EsuEventBuilder
                .withTopic("")
                .setMessage(attachOnAreaChange)
                .buildMessage());
    }

}
