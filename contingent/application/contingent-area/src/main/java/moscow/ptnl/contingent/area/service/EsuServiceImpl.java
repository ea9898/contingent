package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.esu.EsuOutput;
import moscow.ptnl.contingent.area.repository.esu.EsuOutputCRUDRepository;
import moscow.ptnl.contingent.area.model.esu.AreaEvent;
import moscow.ptnl.contingent.area.repository.esu.EsuOutputRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.esu.lib.producer.EsuProducer;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import java.io.StringWriter;
import java.lang.invoke.MethodHandles;
import java.time.Instant;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Objects;
import java.util.TimeZone;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;

@Component
public class EsuServiceImpl implements EsuService {

    private final static Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    private final static String AREA_TOPIC = "SelfContingentArea";

    @Autowired
    private EsuProducer esuProducer;

    @Autowired
    private EsuOutputCRUDRepository esuOutputCRUDRepository;

    @Autowired
    private EsuOutputRepository esuOutputRepository;

    /**
     * Сохраняет информацию о событии в БД и пытается отправить ее в ЕСУ.
     * В случае успеха отправки в ЕСУ, ответ сохраняется в БД.
     *
     * @param event
     * @return true при успешной публикации в ЕСУ
     */
    public boolean saveAndPublishToESU(moscow.ptnl.contingent.area.model.esu.AreaEvent event) {
        String publishTopic = AREA_TOPIC;
        String message = getMessage(event);
        EsuOutput record = saveBeforePublishToESU(publishTopic, message);

        try {
            EsuProducer.MessageMetadata esuAnswer = publishToESU(publishTopic, message);
            saveAfterPublishToESU(record, esuAnswer);
            return true;
        } catch (Exception e) {
            LOG.warn("ошибка при публикации данных о событии в ЕСУ", e);
        }
        return false;
    }

    public void periodicalPublishUnsuccessMessagesToESU(LocalDateTime olderThen) {
        List<EsuOutput> records = esuOutputRepository.findEsuOutputsToResend(olderThen);

        for (EsuOutput record : records) {
            try {
                EsuProducer.MessageMetadata esuAnswer = publishToESU(record.getTopic(), record.getMessage());
                saveAfterPublishToESU(record, esuAnswer);
            } catch (Exception e) {
                LOG.warn("ошибка при публикации данных о событии в ЕСУ", e);
            }
        }
        //меняем статус сообщений на неуспешный, если они еще в статусе inProgress
        records.stream().filter(r -> Objects.equals(2, r.getStatus())).forEach(r -> r.setStatus(0));
    }

    private EsuOutput saveBeforePublishToESU(String publishTopic, String message) {
        EsuOutput esuOutput = new EsuOutput();
        esuOutput.setTopic(publishTopic);
        esuOutput.setMessage(message);
        esuOutput.setStatus(0);

        return esuOutputCRUDRepository.save(esuOutput);
    }

    private void saveAfterPublishToESU(EsuOutput esuOutput, EsuProducer.MessageMetadata esuAnswer) {
        esuOutput.setEsuId(esuAnswer.getKey());
        esuOutput.setOffset(esuAnswer.getOffset());
        esuOutput.setPartition(esuAnswer.getPartition());
        esuOutput.setSentTime(LocalDateTime.ofInstant(Instant.ofEpochMilli(esuAnswer.getTimestamp()), TimeZone.getDefault().toZoneId()));
        esuOutput.setStatus(1);
    }

    private EsuProducer.MessageMetadata publishToESU(String publishTopic, String message) throws InterruptedException, ExecutionException {
        return esuProducer.publish(publishTopic, message);
    }

    private moscow.ptnl.contingent.area.event.AreaEvent mapAreaEvent(AreaEvent event) {
        Area area = event.getArea();
        moscow.ptnl.contingent.area.event.AreaEvent areaEvent = new moscow.ptnl.contingent.area.event.AreaEvent();
        areaEvent.setAreaId(area.getId());
        areaEvent.setMoId(area.getMoId());
        areaEvent.setMuId(area.getMuId());
        areaEvent.setAreaTypeCode(area.getAreaType() == null ? null : area.getAreaType().getCode());
        areaEvent.setAreaName(area.getName());
        areaEvent.setAreaNumber(area.getNumber());
        areaEvent.setKindAreaTypeCode(area.getAreaType() == null || area.getAreaType().getKindAreaType() == null
                ? null : area.getAreaType().getKindAreaType().getCode());
        areaEvent.setClassAreaTypeCode(area.getAreaType() == null || area.getAreaType().getClassAreaType() == null
                ? null : area.getAreaType().getClassAreaType().getCode());
        areaEvent.setAgeMin(area.getAgeMin());
        areaEvent.setAgeMax(area.getAgeMax());
        areaEvent.setAgeMinM(area.getAgeMMin());
        areaEvent.setAgeMaxM(area.getAgeMMax());
        areaEvent.setAgeMinW(area.getAgeWMin());
        areaEvent.setAgeMaxW(area.getAgeWMax());
        areaEvent.setIsAutoAssignForAttach(area.getAutoAssignForAttach());
        areaEvent.setAttachByMedicalReason(area.getAttachByMedicalReason());

        areaEvent.setPrimaryAreaTypes(mapPrimaryAreaTypes(event));

        areaEvent.setOperationDate(getCurrentDate());
        areaEvent.setOperationType(event.getOperationType().getValue());

        return areaEvent;
    }

    private moscow.ptnl.contingent.area.event.AreaEvent.PrimaryAreaTypes mapPrimaryAreaTypes(AreaEvent event) {
        if (event.getPrimaryAreaTypes() != null && !event.getPrimaryAreaTypes().isEmpty()) {
            moscow.ptnl.contingent.area.event.AreaEvent.PrimaryAreaTypes areaTypes =
                    new moscow.ptnl.contingent.area.event.AreaEvent.PrimaryAreaTypes();
            areaTypes.getPrimaryAreaTypeCode().addAll(event.getPrimaryAreaTypes().stream()
                    .filter(a -> a.getAreaType() != null)
                    .map(a -> a.getAreaType().getCode())
                    .collect(Collectors.toList()));
            return areaTypes;
        }
        return null;
    }

    private String getMessage(AreaEvent event) {
        Object publishObject = mapAreaEvent(event);

        return convertEventObjectToMessage(publishObject);
    }

    private static String convertEventObjectToMessage(Object o) {
        String xmlContent = null;

        try {
            JAXBContext jaxbContext = JAXBContext.newInstance(moscow.ptnl.contingent.area.event.AreaEvent.class);
            Marshaller jaxbMarshaller = jaxbContext.createMarshaller();
            jaxbMarshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
            StringWriter sw = new StringWriter();
            jaxbMarshaller.marshal(o, sw);
            xmlContent = sw.toString();
        } catch (JAXBException e) {
            LOG.error("Ошибка конвертации объекта в XML", e);
        }
        return xmlContent;
    }

    private static XMLGregorianCalendar getCurrentDate() {
        GregorianCalendar gcal = new GregorianCalendar();
        XMLGregorianCalendar xgcal = null;

        try {
            xgcal = DatatypeFactory.newInstance().newXMLGregorianCalendar(gcal);
        } catch (DatatypeConfigurationException e) {
            LOG.error("Ошибка получения даты", e);
        }
        return xgcal;
    }
}
