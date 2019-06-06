package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.configuration.EventChannelsConfiguration;
import moscow.ptnl.contingent.domain.esu.EsuEventBuilder;
import moscow.ptnl.contingent.service.EsuService;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.model.esu.AreaCreateEvent;
import moscow.ptnl.contingent.area.repository.area.AreaRepository;
import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import moscow.ptnl.contingent2.attachment.changearea.event.AttachOnAreaChange;
import moscow.ptnl.ws.security.UserContextHolder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.messaging.MessageChannel;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

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
                .withTableAndObject()
                .setPrincipal(UserContextHolder.getPrincipal())
                .addTopic(areaInfoEvent)
                .buildMessage());
    }

    public void sendAttachOnAreaChangeTopicToEsu(AttachOnAreaChange attachOnAreaChange) {
        esuChannel.send(EsuEventBuilder
                .withTableAndObject()
                .setPrincipal(UserContextHolder.getPrincipal())
                .addTopic(attachOnAreaChange)
                .buildMessage());
    }

}
