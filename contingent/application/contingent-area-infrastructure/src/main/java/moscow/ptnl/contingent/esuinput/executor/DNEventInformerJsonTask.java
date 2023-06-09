package moscow.ptnl.contingent.esuinput.executor;

import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.esu.EsuEventBuilder;
import moscow.ptnl.contingent.domain.esu.event.input.DnEventInformer;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeKindEnum;
import moscow.ptnl.contingent.domain.area.repository.AreaRepository;
import moscow.ptnl.contingent.util.XMLGregorianCalendarMapper;
import moscow.ptnl.contingent2.rmr.event.dn.DnAttach;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.messaging.MessageChannel;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.UUID;

import static moscow.ptnl.contingent.area.configuration.EventChannelsConfiguration.ESU_EVENT_CHANNEL_NAME;
import moscow.ptnl.contingent.batch.BaseTopicExecutor;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 * К_УУ_ЕСУ_6.
 * Формирование сообщения для ЕСУ «Уведомление о постановке и снятии с ДН»
 */
//@Component
//@Qualifier(DNEventInformerJsonTask.TASK_NAME)
public class DNEventInformerJsonTask extends BaseTopicExecutor<DnEventInformer> {
    
    public static final String TASK_NAME = "dnEventInformerTask";

    @Value("${esu.consumer.topic.dn.event}")
    private String dnEventReceived;

    @Value("${esu.producer.topic.dn.attach}")
    private String dnAttachTopicSend;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired @Qualifier(ESU_EVENT_CHANNEL_NAME)
    private MessageChannel esuChannel;

    public DNEventInformerJsonTask() {
        super(null, DnEventInformer.class);
    }

    @Override
    protected boolean isMessageTypeJson() {
        return true;
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
    @Transactional(propagation = Propagation.MANDATORY)
    public void processMessage(DnEventInformer event) {

        if ("create".equals(event.getOperationType()) && event.getNewResults() != null) {
            DnEventInformer.Results param = event.getNewResults().get(0);
            List<Area> areas = areaRepository.findAreas(
                    AreaTypeKindEnum.PERSONAL.getCode(), false, Long.valueOf(param.getJobId()));
            if (areas.isEmpty()) {
                throw new RuntimeException("Участок не найден");
            }
            if (areas.size() > 1) {
                throw new RuntimeException("Найдено несколько участков");
            }
            DnAttach dnAttach = new DnAttach();
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
            throw new RuntimeException("Отсутствуют или некорректные данные для обработки");
        }
    }

    @Override
    public String getBeanName() {
        return DNEventInformerJsonTask.TASK_NAME;
    }
}
