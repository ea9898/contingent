package moscow.ptnl.contingent.service.impl;

import moscow.ptnl.contingent.domain.esu.EsuOutput;
import moscow.ptnl.contingent.area.transform.model.esu.AttachOnAreaRelationChangeEventMapper;
import moscow.ptnl.contingent.repository.esu.EsuOutputCRUDRepository;
import moscow.ptnl.contingent.repository.esu.EsuOutputRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import ru.mos.emias.esu.lib.producer.EsuProducer;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import java.io.StringWriter;
import java.lang.invoke.MethodHandles;
import java.time.Instant;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;
import java.util.TimeZone;
import java.util.concurrent.ExecutionException;
import moscow.ptnl.contingent.service.EsuService;
import org.springframework.stereotype.Service;

@Service
public class EsuServiceImpl implements EsuService {

    private final static Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    private final static String AREA_TOPIC = "SelfContingentArea";

    @Autowired
    private EsuProducer esuProducer;

    @Autowired
    private EsuOutputCRUDRepository esuOutputCRUDRepository;

    @Autowired
    private EsuOutputRepository esuOutputRepository;

    @Autowired
    private AttachOnAreaRelationChangeEventMapper attachOnAreaRelationChangeEventMapper;

    /**
     * Сохраняет информацию о событии в БД и пытается отправить ее в ЕСУ.
     * В случае успеха отправки в ЕСУ, ответ сохраняется в БД.
     *
     * @param event
     * @return true при успешной публикации в ЕСУ
     */
    public boolean saveAndPublishToESU(moscow.ptnl.contingent.area.model.esu.AreaEvent event) {
//        String publishTopic = AREA_TOPIC;
//        AttachOnAreaRelationChangeEvent publishObject = attachOnAreaRelationChangeEventMapper.entityToDtoTransform(event);
//        String message = convertEventObjectToMessage(publishObject);
//        EsuOutput record = saveBeforePublishToESU(publishTopic, message);
//        //Обновим ИД сообщения
//        publishObject.setId(record.getId());
//        message = convertEventObjectToMessage(publishObject);
//
//        try {
//            EsuProducer.MessageMetadata esuAnswer = publishToESU(publishTopic, message);
//            saveAfterPublishToESU(record, esuAnswer);
//            return true;
//        } catch (Exception e) {
//            LOG.warn("ошибка при публикации данных о событии в ЕСУ", e);
//        }
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

//    private static String convertEventObjectToMessage(Object o) {
//        String xmlContent = null;
//
//        try {
//            JAXBContext jaxbContext = JAXBContext.newInstance(moscow.ptnl.contingent2.attachment.changedeparea.event.AttachOnAreaRelationChangeEvent.class);
//            Marshaller jaxbMarshaller = jaxbContext.createMarshaller();
//            jaxbMarshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
//            StringWriter sw = new StringWriter();
//            jaxbMarshaller.marshal(o, sw);
//            xmlContent = sw.toString();
//        } catch (JAXBException e) {
//            LOG.error("Ошибка конвертации объекта в XML", e);
//        }
//        return xmlContent;
//    }
}
