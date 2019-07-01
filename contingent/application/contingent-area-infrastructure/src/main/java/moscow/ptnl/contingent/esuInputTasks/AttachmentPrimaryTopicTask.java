package moscow.ptnl.contingent.esuInputTasks;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeClassEnum;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeKindEnum;
import moscow.ptnl.contingent.domain.esu.EsuInput;
import moscow.ptnl.contingent.domain.esu.EsuStatusType;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaRepository;
import moscow.ptnl.contingent.repository.esu.EsuInputCRUDRepository;
import moscow.ptnl.contingent.repository.esu.EsuInputRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.service.esu.EsuService;
import moscow.ptnl.contingent.util.EsuTopicsEnum;
import moscow.ptnl.contingent2.attachment.changeprimarea.event.AttachPrimaryPatientEvent;
import moscow.ptnl.contingent2.attachment.deparea.event.AttachToDependentAreaEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.xml.sax.SAXException;

import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import java.io.StringReader;
import java.lang.invoke.MethodHandles;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;

/**
 * К_УУ_ЕСУ_4
 * Формирование топика «Создать прикрепление к зависимому участку»
 */
@Component
@Scope(proxyMode = ScopedProxyMode.TARGET_CLASS)
public class AttachmentPrimaryTopicTask implements Tasklet {

    @Autowired
    private EsuInputRepository esuInputRepository;

    @Autowired
    private EsuInputCRUDRepository esuInputCRUDRepository;

    @Autowired
    private AreaTypesCRUDRepository areaTypesCRUDRepository;

    @Autowired
    private AreaCRUDRepository areaCRUDRepository;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired
    private EsuService esuService;

    private static final Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    private static final String XSD_PATH = "META-INF/xsd/esu/attachmentprimary.v1.xsd";

    public AttachmentPrimaryTopicTask() {
    }

    @Transactional(propagation = Propagation.REQUIRED)
    public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) throws Exception {
        LOG.info("AttachmentPrimaryTopicTask start..");

        // 1
        List<EsuInput> messages = esuInputRepository.findByTopic(EsuTopicsEnum.ATTACHMENT_PRIMARY.getName());

        for (EsuInput message : messages) {
            AttachPrimaryPatientEvent event = convertAttachment(message);
            // 2
            if (event == null) {
                updateStatus(message, EsuStatusType.UNSUCCESS,null, "Некорректные данные");
                continue;
            }

            List<EsuInput> repeatedMessages = esuInputRepository.findByEventId(String.valueOf(event.getId()));
            if (!repeatedMessages.isEmpty()) {
                updateStatus(message, EsuStatusType.UNSUCCESS, String.valueOf(event.getId()), "Повторное сообщение");
                continue;
            }

            // 3.1
            AreaType areaType = areaTypesCRUDRepository.findById(event.getPrimaryAreaId()).orElse(null);
            // 3.2
            if (areaType == null || !areaType.getAreaTypeClass().getCode().equals(AreaTypeClassEnum.PRIMARY.getClazz())) {
                updateStatus(message, EsuStatusType.UNSUCCESS, String.valueOf(event.getId()), "Тип участка не первичный");
                continue;
            }
            // 3.3
            Area area = areaCRUDRepository.findById(event.getPrimaryAreaId()).orElse(null);
            if (area != null) {
                // 3.4.1
                List<Area> dependentAreas = areaRepository.findAreasWithNotAreaTypeKindCode(null, area.getMuId(),
                        areaType.getCode(), AreaTypeKindEnum.DEPERSONALIZED.getCode(), null, true);
                // 3.4.2
                dependentAreas.addAll(areaRepository.findAreasWithMuIdNullAndNotAreaTypeKindCode(
                        area.getMoId(), areaType.getCode(), AreaTypeKindEnum.DEPERSONALIZED.getCode(), null, true));
                // 3.4.3
                if (dependentAreas.isEmpty()) {
                    updateStatus(message, EsuStatusType.UNSUCCESS,String.valueOf(event.getId()), "Зависимые участки не найдены");
                    continue;
                }
                // 3.5
                AttachToDependentAreaEvent eventDto = AttachToDependentAreaEventMapper.entityToDtoTransform(area, areaType, dependentAreas);
                // 4
                esuService.saveAndPublishToESU(EsuTopicsEnum.ATTACH_TO_DEPENDENT_AREA.getName(), eventDto);
                // 5
                updateStatus(message, EsuStatusType.SUCCESS, String.valueOf(event.getId()),null);
            }
        }

        LOG.info("AttachmentPrimaryTopicTask done..");
        return RepeatStatus.FINISHED;
    }

    private AttachPrimaryPatientEvent convertAttachment(EsuInput message) {
        JAXBContext jaxbContext;
        AttachPrimaryPatientEvent attachPrimaryPatientEvent = null;
        try {
            jaxbContext = JAXBContext.newInstance(AttachPrimaryPatientEvent.class);
            SchemaFactory sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            Schema schema = sf.newSchema(Objects.requireNonNull(
                    Thread.currentThread().getContextClassLoader().getResource(XSD_PATH)));
            Unmarshaller jaxbUnmarshaller = jaxbContext.createUnmarshaller();
            jaxbUnmarshaller.setSchema(schema);
            StringReader reader = new StringReader(message.getMessage());
            attachPrimaryPatientEvent = (AttachPrimaryPatientEvent) jaxbUnmarshaller.unmarshal(reader);
        } catch (JAXBException | SAXException e) {
            LOG.error("Некорректные данные", e.getMessage());
        }
        return attachPrimaryPatientEvent;
    }

    private void updateStatus(EsuInput message, EsuStatusType status, String eventId, String errorMessage) {
        if (status.equals(EsuStatusType.UNSUCCESS)) {
            message.setErrorMessage(errorMessage);
        }
        if (eventId != null) {
            message.setEventId(eventId);
        }
        message.setStatus(status);
        message.setUpdateTime(LocalDateTime.now());
        esuInputCRUDRepository.save(message);
    }
}