package moscow.ptnl.contingent.esuInputTasks;

import moscow.ptnl.contingent.area.configuration.EventChannelsConfiguration;
import moscow.ptnl.contingent.domain.esu.EsuEventBuilder;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaRepository;
import moscow.ptnl.contingent2.rmr.event.DnEventInformer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.messaging.MessageChannel;
import org.springframework.stereotype.Component;

import java.util.UUID;

/**
 * К_УУ_ЕСУ_6
 * Формирование сообщения для ЕСУ «Уведомление о постановке и снятии с ДН»
 */
@Component
@Qualifier("dnEventInformerTask")
public class DNEventInformerTask extends BaseTopicTask<DnEventInformer> {

    @Value("${esu.consumer.topic.dn.event}")
    private String dnEventReceived;

    @Value("${esu.producer.topic.dn.attach}")
    private String dnAttachTopicSend;

    @Autowired
    private AreaCRUDRepository areaCRUDRepository;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired @Qualifier(EventChannelsConfiguration.ESU_EVENT_CHANNEL_NAME)
    private MessageChannel esuChannel;

    private static final String XSD_PATH = "META-INF/xsd/esu/dn.attach.xsd";

    public DNEventInformerTask() {
        super(XSD_PATH, DnEventInformer.class);
    }

    @Override
    public String getTopicName() {
        return dnEventReceived;
    }

    @Override
    protected String getEventId(DnEventInformer event) {
        return UUID.randomUUID().toString();
    }

    @Override
    public void processMessage(DnEventInformer event) {

            esuChannel.send(EsuEventBuilder
                    .withTopic(dnAttachTopicSend)
                    .setEventObject(event)
                    .buildMessage());
    }
}