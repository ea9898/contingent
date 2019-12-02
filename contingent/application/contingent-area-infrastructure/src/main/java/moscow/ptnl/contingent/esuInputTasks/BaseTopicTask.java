package moscow.ptnl.contingent.esuInputTasks;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.ObjectMapper;
import moscow.ptnl.contingent.domain.esu.EsuInput;
import moscow.ptnl.contingent.domain.esu.EsuStatusType;
import moscow.ptnl.contingent.repository.esu.EsuInputRepository;
import moscow.ptnl.contingent.service.TransactionRunner;
import moscow.ptnl.util.CommonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import java.lang.invoke.MethodHandles;
import java.time.LocalDateTime;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import moscow.ptnl.util.XMLUtil;

/**
 * Базовый класс обработчика входящих сообщений ЕСУ
 */
@PropertySource("classpath:application-esu.properties")
abstract class BaseTopicTask<T> implements Tasklet {

    private static final Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    @Autowired
    private EsuInputRepository esuInputRepository;

    @Autowired
    private TransactionRunner transactionRunner;

    @Value("${esu.consumer.group.id}")
    private String consumerGroupId;

    private final Class<T> typeClass;

    private final String xsdPath;

    private final String host;

    private final ObjectMapper jsonMapper;

    BaseTopicTask(String xsdPath, Class<T> typeClass) {
        this.xsdPath = xsdPath;
        this.typeClass = typeClass;
        this.host = CommonUtils.getHostName();
        this.jsonMapper = new ObjectMapper();
        this.jsonMapper.configure(JsonParser.Feature.ALLOW_COMMENTS, true);
    }

    @Override
    @Transactional(propagation = Propagation.REQUIRED)
    final public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) throws Exception {
        LOG.info(typeClass.getSimpleName() + " task start");

        String personalTopic = getTopicName() + "." + consumerGroupId;
        List<EsuInput> messages = esuInputRepository.findByTopic(getTopicName(), personalTopic);

        for (EsuInput message : messages) {
            String eventId = null;

            try {
                T event = isMessageTypeJson() ? convertEventJson(message.getMessage()) : convertEventXml(message.getMessage());
                eventId = getEventId(event);

                //Выполняем в новой транзакции, чтобы можно было сохранить результат операции при ошибке
                Future future = CompletableFuture.runAsync(() -> transactionRunner.run(() -> processMessage(event)));

                try {
                    // Если за 5 секунд не закомителось, то что то идет не так...
                    future.get(5, TimeUnit.SECONDS);
                }
                catch (InterruptedException ex) {
                    break;
                }
                catch (ExecutionException ex) {
                    throw ex.getCause();
                }
                updateStatus(message, EsuStatusType.SUCCESS, eventId, null);
            }
            catch (Throwable ex) {
                LOG.debug("Ошибка обработки входящего сообщения ЕСУ", ex);
                updateStatus(message, EsuStatusType.UNSUCCESS, eventId, ex.getMessage());
            }
        }
        LOG.info(typeClass.getSimpleName() + " task done");

        return RepeatStatus.FINISHED;
    }

    protected abstract String getEventId(T event);

    protected boolean isMessageTypeJson() {
        return false;
    }

    public abstract void processMessage(T event);

    public abstract String getTopicName();

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
        if (status.equals(EsuStatusType.UNSUCCESS)) {
            message.setErrorMessage(errorMessage);
        }
        if (eventId != null) {
            message.setEventId(eventId);
        }
        message.setStatus(status);
        message.setUpdateTime(LocalDateTime.now());
        message.setHost(host);
    }
}