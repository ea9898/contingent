package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.esu.EsuOutput;
import moscow.ptnl.contingent.area.repository.esu.EsuOutputCRUDRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.esu.lib.producer.EsuProducer;

import java.io.Serializable;
import java.lang.invoke.MethodHandles;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;

@Component
public class EsuServiceImpl implements EsuService {

    private final static Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    @Autowired
    private EsuProducer esuProducer;

    @Autowired
    private EsuOutputCRUDRepository esuOutputCRUDRepository;

    /**
     * Сохраняет информацию о событии в БД и пытается отправить ее в ЕСУ.
     * В случае успеха отправки в ЕСУ, ответ сохраняется в БД.
     *
     * @param data
     * @return true при успешной публикации в ЕСУ
     */
    public boolean saveAndPublishToESU(Serializable data) {
//        String publishTopic = getPublishTopic(event);
//        String message = getMessage(publishTopic, event.getEventDataIspk());
//        long recordId = saveBeforePublishToESU(publishTopic, message);
//        try {
//            EsuProducer.MessageMetadata esuAnswer = publishToESU(publishTopic, message);
//            saveAfterPublishToESU(recordId, esuAnswer);
//            return true;
//        } catch (Exception e) {
//            LOG.warn("ошибка при публикации данных о событии в ЕСУ", e);
//        }
        return false;
    }

    public void periodicalPublishUnsuccessMessagesToESU(Date olderThen) {
        /*List<Long> recordsIds = producerRepository.updateLogMessagesInProgressStatus(olderThen);
        if (!recordsIds.isEmpty()) {
            Map<Long, String[]> messages = producerRepository.getLogMessages(recordsIds);
            for (long recordId : recordsIds) {
                try {
                    String[] messageData = messages.get(recordId);
                    EsuProducer.MessageMetadata esuAnswer = publishToESU(messageData[0], messageData[1]);
                    saveAfterPublishToESU(recordId, esuAnswer);
                } catch (Exception e) {
                    LOG.warn("ошибка при публикации данных о событии в ЕСУ", e);
                }
            }
            producerRepository.updateLogMessagesInUnsuccessStatus(recordsIds); //меняем статус сообщений на неуспешный, если они еще в статусе inProgress
        }
        */
    }

    private Long saveBeforePublishToESU(String publishTopic, String message) {
        EsuOutput esuOutput = new EsuOutput();
        esuOutput.setTopic(publishTopic);
        esuOutput.setMessage(message);
        esuOutput.setStatus(0);

        return esuOutputCRUDRepository.save(esuOutput).getId();
    }

    private void saveAfterPublishToESU(long recordId, EsuProducer.MessageMetadata esuAnswer) {
        EsuOutput esuOutput = esuOutputCRUDRepository.findById(recordId).get();
        esuOutput.setEsuId(esuAnswer.getKey());
        esuOutput.setOffset(esuAnswer.getOffset());
        esuOutput.setPartition(esuAnswer.getPartition());
        esuOutput.setSentTime(new Date(esuAnswer.getTimestamp()));
        esuOutput.setStatus(1);
    }

    private EsuProducer.MessageMetadata publishToESU(String publishTopic, String message) throws InterruptedException, ExecutionException {
        return esuProducer.publish(publishTopic, message);
    }

    /*private String getMessage(String publishTopic, EventDataIspk event) {
        if (NotificationEventRepository.EVENT_TYPE_ATTACHMENT.equals(publishTopic)) {
            Object publishObject = mapIspkToAttachmentEvent(event);
            return NotificationEventRepository.convertEventObjectToMessage(publishObject);
        } else if (NotificationEventRepository.EVENT_TYPE_SUBSCTIBTIONS.equals(publishTopic)) {
            Object publishObject = mapIspkToSubscribeEvent(event);
            return NotificationEventRepository.convertEventObjectToMessage(publishObject);
        }
        return null;
    }
    */
}
