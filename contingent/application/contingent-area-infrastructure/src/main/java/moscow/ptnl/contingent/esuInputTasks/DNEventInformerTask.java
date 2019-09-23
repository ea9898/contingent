package moscow.ptnl.contingent.esuInputTasks;

import moscow.ptnl.contingent.area.configuration.EventChannelsConfiguration;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.domain.esu.EsuEventBuilder;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeKindEnum;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaRepository;
import moscow.ptnl.contingent.repository.esu.EsuInputRepository;
import moscow.ptnl.contingent.util.XMLGregorianCalendarMapper;
import moscow.ptnl.contingent2.rmr.event.DnAttach;
import moscow.ptnl.contingent2.rmr.event.DnEventInformer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.messaging.MessageChannel;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.List;
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

        if ("create".equals(event.getOperationType()) && event.getNewResults() != null) {
            DnEventInformer.NewResults param = event.getNewResults();
            List<Area> areas = areaRepository.findAreas(
                    AreaTypeKindEnum.PERSONAL.getCode(), false, Long.valueOf(param.getJobId()));
            if (areas.isEmpty()) {
                throw new RuntimeException("Участок не найден");
            }
            if (areas.size() > 1) {
                throw new RuntimeException("Найдено несколько участков");
            }
            DnAttach dnAttach = new DnAttach();
            dnAttach.setId(UUID.randomUUID().toString());
            dnAttach.setOperationDate(XMLGregorianCalendarMapper.getNow());
            dnAttach.setPatientEmiasId(Long.valueOf(param.getPatientEmiasId()));
            dnAttach.setSimiDocumentId(param.getDocumentId());
            Area area = areas.get(0);
            if (param.isIsAttached()) {
                DnAttach.CreateAttachment createAttach = new DnAttach.CreateAttachment();
                createAttach.setMoId(area.getMoId());
                createAttach.setAreaId(area.getId());
                createAttach.setNotForSelfAppointment(param.isIsNotForSelfAppointment());
                dnAttach.setCreateAttachment(createAttach);
            } else {
                DnAttach.CloseAttachment closeAttach = new DnAttach.CloseAttachment();
                closeAttach.setAreaId(area.getId());
                dnAttach.setCloseAttachment(closeAttach);
            }
            esuChannel.send(EsuEventBuilder
                    .withTopic(dnAttachTopicSend)
                    .setEventObject(dnAttach)
                    .buildMessage());
        } else {
            throw new RuntimeException("Отсутствуют данные для обработки");
        }

    }
}