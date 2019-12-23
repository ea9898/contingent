package moscow.ptnl.contingent.infrastructure.service;

import moscow.ptnl.contingent.configuration.AsyncEsuExecutor;
import moscow.ptnl.contingent.domain.esu.ESUEventHelper;
import moscow.ptnl.contingent.domain.esu.EsuOutput;
import moscow.ptnl.contingent.domain.esu.EsuStatusType;
import moscow.ptnl.contingent.repository.esu.EsuOutputCRUDRepository;
import moscow.ptnl.contingent.repository.esu.EsuOutputRepository;
import moscow.ptnl.util.CommonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import ru.mos.emias.esu.lib.producer.EsuProducer;

import java.lang.invoke.MethodHandles;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Service;
import ru.mos.emias.esu.lib.producer.MessageMetadata;

import javax.annotation.PostConstruct;

@Service
@PropertySource("classpath:application-esu.properties")
public class EsuServiceImpl implements EsuService {

    private final static Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    @Autowired @Lazy
    private EsuProducer esuProducer;

    @Autowired
    private EsuOutputCRUDRepository esuOutputCRUDRepository;
    
    @Autowired
    private EsuOutputRepository esuOutputRepository;
        
    @Value("${esu.producer.produce.timeout}")
    private Integer producerRequestTimeout;
    
    @Autowired
    private AsyncEsuExecutor asyncEsuExecutor;

    private String host;

    @PostConstruct
    public void initHostName() {
        host = CommonUtils.getHostName();
    }

    /**
     * Сохраняет информацию о событии в БД и пытается отправить ее в ЕСУ.
     * В случае успеха отправки в ЕСУ, ответ сохраняется в БД.
     *
     * @param topicName
     * @param event
     * @return true при успешной публикации в ЕСУ
     */
    @Override
    public boolean saveAndPublishToESU(String topicName, Object event) {
        LOG.debug("create message for topic: {}", topicName);
        
        try {
            EsuOutput esuOutput = new EsuOutput();
            esuOutput.setTopic(topicName);
            esuOutput.setSentTime(LocalDateTime.now());
            esuOutput.setStatus(EsuStatusType.INPROGRESS);
            esuOutput.setMessage(" ");
            esuOutput = esuOutputCRUDRepository.save(esuOutput);
            
            String message = ESUEventHelper.toESUMessage(event, esuOutput.getId());
            LOG.debug(message);
            esuOutput.setMessage(message);
            esuOutput.setMethod(ESUEventHelper.resolveMethodName(event));
            esuOutputRepository.updateMessage(esuOutput.getId(), message);
            
            //здесь нужна асинхронность, чтобы не блокировать базу             
            asyncEsuExecutor.publishToESU(this, esuOutput.getId(), esuOutput.getTopic(), esuOutput.getMessage());
            
            return true;
        } catch (Exception e) {
            LOG.error("ошибка при создании события для ЕСУ", e);
        }
        
        return false;
    }

    @Override
    public void periodicalPublishUnsuccessMessagesToESU(LocalDateTime olderThen) {
        
        List<EsuOutput> records = esuOutputRepository.findEsuOutputsToResend(olderThen);

        records.forEach((record) -> {
            LOG.debug("TRY RESEND TO ESU: {}", record.getId());
            //здесь нужна асинхронность, чтобы не блокировать базу
            asyncEsuExecutor.publishToESU(this, record.getId(), record.getTopic(), record.getMessage());
        });
    }

    /**
     * Блокирующий метод публикации в ЕСУ.
     * 
     * @param recordId
     * @param publishTopic
     * @param message 
     */
    @Override
    public void publishToESU(final Long recordId, final String publishTopic, final String message) {
                
        MessageMetadata esuAnswer = null;
        //запускаем в потоке чтобы иметь возможность прервать по таймауту
        try {
            esuAnswer = CompletableFuture.supplyAsync(() -> {
                try {
                    return esuProducer.publish(publishTopic, message);
                } catch (Exception e){
                    LOG.error("ошибка при публикации данных в ЕСУ", e);
                }
                return null;
            } /*,getPublishThreadExecutor()*/).get(producerRequestTimeout, TimeUnit.MILLISECONDS);
        } catch (Throwable e) {
            LOG.error("публикации данных в ЕСУ прервана", e);
        }
        
        try {
            if (esuAnswer == null) {
                esuOutputRepository.updateStatus(recordId, EsuStatusType.INPROGRESS, EsuStatusType.UNSUCCESS, host);
            } else {
                Optional<EsuOutput> result = esuOutputCRUDRepository.findById(recordId);
                if (result.isPresent()) {
                    EsuOutput esuOutput = result.get();
                    esuOutput.setEsuId(esuAnswer.getKey());
                    esuOutput.setOffset(esuAnswer.getOffset());
                    esuOutput.setPartition(esuAnswer.getPartition());
                    esuOutput.setSentTime(toLocalDateTime(esuAnswer.getTimestamp()));
                    esuOutput.setStatus(EsuStatusType.SUCCESS);
                    esuOutput.setHost(host);
                    esuOutputCRUDRepository.save(esuOutput);
                } else {
                    LOG.warn("не найдено событие в ESU_OUTPUT с идентификатором: {}", recordId);
                }
            }
        } catch (Exception e) {
            LOG.error("ошибка при публикации данных о событии в ЕСУ", e);
            esuOutputRepository.updateStatus(recordId, EsuStatusType.INPROGRESS, EsuStatusType.UNSUCCESS, host);
        }
    }
    
    private static LocalDateTime toLocalDateTime(long timestamp) {
        return LocalDateTime.ofInstant(Instant.ofEpochSecond(timestamp / 1000), ZoneOffset.systemDefault());
    }
    
    /**
     * Позволяет запускать поток выполнения как демон.
     * 
     * @return 
     */
    private static ExecutorService getPublishThreadExecutor() {
        final ThreadFactory threadFactory = (Runnable r) -> {
            Thread t = Executors.defaultThreadFactory().newThread(r);
            t.setDaemon(true);
            t.setName("PublishToESU Thread");
            return t;
        };
        return Executors.newFixedThreadPool(1, threadFactory);
    }
}
