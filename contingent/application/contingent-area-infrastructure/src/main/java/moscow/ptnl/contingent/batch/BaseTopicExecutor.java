package moscow.ptnl.contingent.batch;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import moscow.ptnl.contingent.domain.esu.EsuInput;
import moscow.ptnl.contingent.domain.esu.EsuStatusType;
import moscow.ptnl.contingent.infrastructure.service.TransactionRunService;
import moscow.ptnl.contingent.repository.esu.EsuInputRepository;
import moscow.ptnl.util.CommonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import java.time.LocalDateTime;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import moscow.ptnl.util.XMLUtil;

/**
 * Базовый класс обработчика входящих сообщений ЕСУ.
 * 
 * @param <T>
 */
@PropertySource("classpath:application-esu.properties")
public abstract class BaseTopicExecutor<T> {

    private static final Logger LOG = LoggerFactory.getLogger(BaseTopicExecutor.class);

    @Autowired
    private EsuInputRepository esuInputRepository;

    @Autowired
    private TransactionRunService transactionRunner;

    @Value("${esu.consumer.group.id}")
    private String consumerGroupId;

    private final Class<T> typeClass;
    private final String xsdPath;
    private final String host;
    private final ObjectMapper jsonMapper;

    protected BaseTopicExecutor(String xsdPath, Class<T> typeClass) {
        this.xsdPath = xsdPath;
        this.typeClass = typeClass;
        this.host = CommonUtils.getHostName();
        this.jsonMapper = new ObjectMapper();
        this.jsonMapper.configure(JsonParser.Feature.ALLOW_COMMENTS, true);
        this.jsonMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    }
    
    @Transactional(propagation = Propagation.REQUIRED)
    public void execute() {
        String personalTopic = getTopicName() + "." + consumerGroupId;
        List<EsuInput> messages = esuInputRepository.findByTopic(getTopicName(), personalTopic).getContent();

        if (!messages.isEmpty()) {
            Future<?> futureUpdate = null;

            try {
                 futureUpdate = transactionRunner.run(() -> {
                    messages.forEach(msg -> msg.setStatus(EsuStatusType.INPROGRESS));
                    esuInputRepository.saveAll(messages);
                    return true;
                });
                futureUpdate.get(10, TimeUnit.SECONDS);
            } catch (Throwable th) {
                LOG.warn("Ошибка перевода сообщений в статус \"В обработке\". Topic - {}", getTopicName(), th);

                if (th instanceof TimeoutException) {
                    //Завершаем выполнение потока
                    futureUpdate.cancel(true);
                }
                return;
            }
        }
        for (EsuInput message : messages) {
            String eventId = null;
            EsuStatusType status = EsuStatusType.UNSUCCESS;;
            String errorMessage = null;
            Future<?> future = null;
                    
            try {
                T event = isMessageTypeJson() ? convertEventJson(message.getMessage()) : convertEventXml(message.getMessage());
                eventId = getEventId(event);

                //Выполняем в новой транзакции, чтобы можно было сохранить результат операции при ошибке
                future = transactionRunner.run(() -> {
                    processMessage(event);
                    return true;
                });
                // Если за 10 секунд не закомителось, то что то идет не так...
                future.get(10, TimeUnit.SECONDS);

                status = EsuStatusType.SUCCESS;
            } catch (TimeoutException e) {
                LOG.warn("Обработка входящего сообщения ЕСУ с ID={} прервана по таймауту", message.getEsuId(), e);
                //Завершаем выполнение потока
                if (future != null) {
                    future.cancel(true);
                }
                errorMessage = "Обработка прервана по таймауту";
            } catch (Throwable ex) {
                ex = ex instanceof ExecutionException ? ex.getCause() : ex;
                LOG.error("Ошибка обработки входящего сообщения ЕСУ с ID={}", message.getEsuId(), ex);
                ex = ex.getMessage().startsWith("Transaction has thrown an Exception") && ex.getCause() != null ? ex.getCause() : ex;
                errorMessage = ex.getMessage(); 
            } finally {
                updateStatus(message, status, eventId, errorMessage);
            }
        }
    }

    protected abstract String getEventId(T event);

    protected boolean isMessageTypeJson() {
        return false;
    }

    public abstract void processMessage(T event);

    public abstract String getTopicName();
    
    /**
     * Имя бина (квалифаер) реализующего логику обработки. 
     * @return 
     */
    public abstract String getBeanName();

    private T convertEventXml(String message) {
        try {
            return XMLUtil.convertMessageToObject(message, typeClass, xsdPath);
        } catch (Exception ex) {
            throw new RuntimeException("Некорректное входящее XML сообщение", ex);
        }
    }

    private T convertEventJson(String message) {
        try {
            return jsonMapper.readValue(message, typeClass);
        } catch (Exception ex) {
            throw new RuntimeException("Некорректное входящее JSON сообщение", ex);
        }
    }

    private void updateStatus(EsuInput message, EsuStatusType status, String eventId, String errorMessage) {
        try {
            if (EsuStatusType.UNSUCCESS.equals(status)) {
                message.setErrorMessage(errorMessage);
            }
            if (eventId != null) {
                message.setEventId(eventId);
            }
            message.setStatus(status);
            message.setUpdateTime(LocalDateTime.now());
            message.setHost(host);
            
            esuInputRepository.save(message);
        } catch (Throwable er) {
            LOG.error("Ошибка сохранения статуса сообщения ЕСУ с ID={}", message.getEsuId(), er);
        }
    }
}
