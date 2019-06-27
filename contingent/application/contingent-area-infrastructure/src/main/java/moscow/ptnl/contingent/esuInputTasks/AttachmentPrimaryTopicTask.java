package moscow.ptnl.contingent.esuInputTasks;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeClassEnum;
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
        System.out.println("MyTaskOne start..");

        List<EsuInput> messages = esuInputRepository.findByTopic(EsuTopicsEnum.ATTACHMENT_PRIMARY.getName());

        for (EsuInput message : messages) {
            AttachPrimaryPatientEvent event = convertAttachment(message.getMessage());
            AreaType areaType = areaTypesCRUDRepository.findById(event.getPrimaryAreaId()).orElse(null);
            if (areaType == null || !areaType.getAreaTypeClass().getCode().equals(AreaTypeClassEnum.PRIMARY.getClazz())) {
                message.setErrorMessage("Тип участка не первичный");
                message.setStatus(EsuStatusType.UNSUCCESS);
                message.setUpdateTime(LocalDateTime.now());
                esuInputCRUDRepository.save(message);
                continue;
            }
            Area area = areaCRUDRepository.findById(event.getPrimaryAreaId()).orElse(null);
            if (area != null) {
                List<Area> dependentAreas = areaRepository.findAreas(null, area.getMuId(), areaType.getCode(), null, true);
                dependentAreas.addAll(areaRepository.findAreasWithMuIdNull(area.getMoId(), areaType.getCode(), null, true));
                if (dependentAreas.isEmpty()) {
                    message.setErrorMessage("Зависимые участки не найдены");
                    message.setStatus(EsuStatusType.UNSUCCESS);
                    message.setUpdateTime(LocalDateTime.now());
                    esuInputCRUDRepository.save(message);
                    continue;
                }
                AttachToDependentAreaEvent eventDto = AttachToDependentAreaEventMapper.entityToDtoTransform(area, areaType, dependentAreas);
                esuService.saveAndPublishToESU(EsuTopicsEnum.ATTACH_TO_DEPENDENT_AREA.getName(), eventDto);
                message.setUpdateTime(LocalDateTime.now());
                message.setStatus(EsuStatusType.SUCCESS);
                esuInputCRUDRepository.save(message);
            }
        }

        System.out.println("MyTaskOne done..");
        return RepeatStatus.FINISHED;
    }

    private AttachPrimaryPatientEvent convertAttachment(String body) {
        JAXBContext jaxbContext;
        AttachPrimaryPatientEvent attachPrimaryPatientEvent = null;
        try {
            jaxbContext = JAXBContext.newInstance(AttachPrimaryPatientEvent.class);
            SchemaFactory sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            Schema schema = sf.newSchema(Objects.requireNonNull(
                    Thread.currentThread().getContextClassLoader().getResource(XSD_PATH)));
            Unmarshaller jaxbUnmarshaller = jaxbContext.createUnmarshaller();
            jaxbUnmarshaller.setSchema(schema);
            StringReader reader = new StringReader(body);
            attachPrimaryPatientEvent = (AttachPrimaryPatientEvent) jaxbUnmarshaller.unmarshal(reader);
        } catch (JAXBException | SAXException e) {
            LOG.error("Некорректные данные; ", e.getMessage());
        }
        return attachPrimaryPatientEvent;
    }
}