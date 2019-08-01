package moscow.ptnl.contingent.esuInputTasks;

import moscow.ptnl.contingent.domain.esu.EsuInput;
import moscow.ptnl.contingent.domain.esu.EsuStatusType;
import moscow.ptnl.contingent.repository.esu.EsuInputRepository;
import moscow.ptnl.contingent.service.TransactionRunner;
import moscow.ptnl.contingent.util.EsuTopicsEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
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
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

/**
 * Базовый класс обработчика входящих сообщений ЕСУ
 */
abstract class BaseTopicTask<T> implements Tasklet {

    private static final Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    @Autowired
    private EsuInputRepository esuInputRepository;

    @Autowired
    private TransactionRunner transactionRunner;

    @Value("${esu.consumer.group.id}")
    private String consumerGroupId;

    private final Class<T> typeClass;

    private final EsuTopicsEnum topic;

    private final String xsdPath;

    BaseTopicTask(EsuTopicsEnum topic, String xsdPath, Class<T> typeClass) {
        this.topic = topic;
        this.xsdPath = xsdPath;
        this.typeClass = typeClass;
    }

    @Override
    @Transactional(propagation = Propagation.REQUIRED)
    final public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) throws Exception {
        LOG.info(typeClass.getSimpleName() + " task start");

        String personalTopic = topic.getName() + "." + consumerGroupId;
        List<EsuInput> messages = esuInputRepository.findByTopic(topic.getName(), personalTopic);

        for (EsuInput message : messages) {
            String eventId = null;

            try {
                T event = convertEvent(message.getMessage());
                eventId = getEventId(event);

                if (!esuInputRepository.findByEventId(eventId).isEmpty()) {
                    throw new RuntimeException("Повторное сообщение");
                }
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

    public abstract void processMessage(T event);

    @SuppressWarnings("unchecked")
    private T convertEvent(String message) {
        try {
            JAXBContext jaxbContext = JAXBContext.newInstance(typeClass);
            SchemaFactory sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            Schema schema = sf.newSchema(Objects.requireNonNull(Thread.currentThread().getContextClassLoader().getResource(xsdPath)));
            Unmarshaller jaxbUnmarshaller = jaxbContext.createUnmarshaller();
            jaxbUnmarshaller.setSchema(schema);
            StringReader reader = new StringReader(message);

            return (T) jaxbUnmarshaller.unmarshal(reader);
        }
        catch (JAXBException | SAXException ex) {
            throw new RuntimeException("Некорректное входящее сообщение", ex);
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
    }
}