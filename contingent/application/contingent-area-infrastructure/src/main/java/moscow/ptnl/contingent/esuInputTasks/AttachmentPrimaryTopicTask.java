package moscow.ptnl.contingent.esuInputTasks;

import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeClassEnum;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeKindEnum;
import moscow.ptnl.contingent.domain.esu.EsuEventBuilder;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaRepository;
import moscow.ptnl.contingent2.attachment.changeprimarea.event.AttachPrimaryPatientEvent;
import moscow.ptnl.contingent2.attachment.deparea.event.AttachToDependentAreaEvent;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.messaging.MessageChannel;
import org.springframework.stereotype.Component;

import java.util.List;
import static moscow.ptnl.contingent.area.configuration.EventChannelsConfiguration.ESU_EVENT_CHANNEL_NAME;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 * К_УУ_ЕСУ_4
 * Формирование топика «Создать прикрепление к зависимому участку»
 */
@Component
@Qualifier("attachmentPrimaryTopicTask")
public class AttachmentPrimaryTopicTask extends BaseTopicTask<AttachPrimaryPatientEvent> {


    @Value("${esu.consumer.topic.primary.area.attachment}")
    private String attachmentPrimaryMsgTopicName;

    @Value("${topic.attach.to.dependent.area}")
    private String attachToDependentMsgTopicName;

    @Autowired
    private AreaCRUDRepository areaCRUDRepository;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired @Qualifier(ESU_EVENT_CHANNEL_NAME)
    private MessageChannel esuChannel;

    private static final String XSD_PATH = "META-INF/xsd/esu/attachmentprimary.v1.xsd";

    public AttachmentPrimaryTopicTask() {
        super(XSD_PATH, AttachPrimaryPatientEvent.class);
    }


    @Override
    public String getTopicName() {
        return attachmentPrimaryMsgTopicName;
    }

    @Override
    protected String getEventId(AttachPrimaryPatientEvent event) {
        return String.valueOf(event.getId());
    }

    @Override
    @Transactional(propagation = Propagation.MANDATORY)
    public void processMessage(AttachPrimaryPatientEvent event) {
        // 3.1
        Area area = areaCRUDRepository.findById(event.getPrimaryAreaId()).orElse(null);

        if (area != null) {
            AreaType areaType = area.getAreaType();
            // 3.2
            if (areaType == null || !AreaTypeClassEnum.PRIMARY.areaTypeClassEquals(areaType.getAreaTypeClass())) {
                throw new RuntimeException("Тип участка не первичный");
            }
            // 3.4.1
            List<Area> dependentAreas = areaRepository.findDependentAreasByMuMoIdAndType(area.getMuId(), null,
                    areaType.getCode(), AreaTypeKindEnum.PERSONAL.getCode());

            // 3.4.2
            dependentAreas.addAll(areaRepository.findDependentAreasByMuMoIdAndType( null,
                    area.getMoId(), areaType.getCode(), AreaTypeKindEnum.PERSONAL.getCode()));

            // 3.4.3
            if (dependentAreas.isEmpty()) {
                throw new RuntimeException("Зависимые участки не найдены");
            }
            // 3.5
            AttachToDependentAreaEvent eventDto = AttachToDependentAreaEventMapper
                    .entityToDtoTransform(event, dependentAreas);

            // 4
            esuChannel.send(EsuEventBuilder
                    .withTopic(attachToDependentMsgTopicName)
                    .setEventObject(eventDto)
                    .buildMessage());
        } else {
            throw new RuntimeException("Участок %s не найден");
        }
    }
}
