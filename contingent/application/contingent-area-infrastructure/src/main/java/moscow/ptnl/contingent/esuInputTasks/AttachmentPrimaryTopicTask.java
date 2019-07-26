package moscow.ptnl.contingent.esuInputTasks;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeClassEnum;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeKindEnum;
import moscow.ptnl.contingent.configuration.EventChannelsConfiguration;
import moscow.ptnl.contingent.domain.esu.EsuEventBuilder;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.util.EsuTopicsEnum;
import moscow.ptnl.contingent2.attachment.changeprimarea.event.AttachPrimaryPatientEvent;
import moscow.ptnl.contingent2.attachment.deparea.event.AttachToDependentAreaEvent;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.messaging.MessageChannel;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * К_УУ_ЕСУ_4
 * Формирование топика «Создать прикрепление к зависимому участку»
 */
@Component
@Qualifier("attachmentPrimaryTopicTask")
public class AttachmentPrimaryTopicTask extends BaseTopicTask<AttachPrimaryPatientEvent> {

    @Autowired
    private AreaTypesCRUDRepository areaTypesCRUDRepository;

    @Autowired
    private AreaCRUDRepository areaCRUDRepository;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired @Qualifier(EventChannelsConfiguration.ESU_EVENT_CHANNEL_NAME)
    private MessageChannel esuChannel;

    private static final String XSD_PATH = "META-INF/xsd/esu/attachmentprimary.v1.xsd";

    public AttachmentPrimaryTopicTask() {
        super(EsuTopicsEnum.ATTACHMENT_PRIMARY, XSD_PATH, AttachPrimaryPatientEvent.class);
    }

    @Override
    protected String getEventId(AttachPrimaryPatientEvent event) {
        return String.valueOf(event.getId());
    }

    @Override
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
            List<Area> dependentAreas = areaRepository.findAreasWithNotAreaTypeKindCode(null, area.getMuId(),
                    areaType.getCode(), AreaTypeKindEnum.DEPERSONALIZED.getCode(), null, true);
            // 3.4.2
            dependentAreas.addAll(areaRepository.findAreasWithMuIdNullAndNotAreaTypeKindCode(
                    area.getMoId(), areaType.getCode(), AreaTypeKindEnum.DEPERSONALIZED.getCode(), null, true));
            // 3.4.3
            if (dependentAreas.isEmpty()) {
                throw new RuntimeException("Зависимые участки не найдены");
            }
            // 3.5
            AttachToDependentAreaEvent eventDto = AttachToDependentAreaEventMapper.entityToDtoTransform(area, areaType, dependentAreas);
            // 4
            esuChannel.send(EsuEventBuilder
                    .withTopic(EsuTopicsEnum.ATTACH_TO_DEPENDENT_AREA.getName())
                    .setEventObject(eventDto)
                    .buildMessage());
        }
    }
}