package moscow.ptnl.contingent.service.esu;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Service;
import ru.mos.emias.esu.consumer.EsuConsumer;
import ru.mos.emias.esu.consumer.EsuConsumerMessageProcessor;
import ru.mos.emias.esu.consumer.EsuTopicConsumer;

import javax.annotation.PostConstruct;
import javax.management.IntrospectionException;
import javax.management.MalformedObjectNameException;
import javax.management.ReflectionException;
import java.lang.invoke.MethodHandles;

/**
 * Сервис подписки на топики и сохранения в БД сообщений от ЕСУ
 * sorlov
 */
@Service
@PropertySource("classpath:application-esu.properties")
public class EsuConsumerService {

    private static final Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    private EsuTopicConsumer consumerPrimaryAreaAttachment;

    private EsuTopicConsumer consumerJobExecutionInfoMsg;

    @Autowired
    private EsuConsumerDatabaseProcessor esuConsumerDatabaseProcessor;

    @Value("${esu.consumer.topic.primary.area.attachment}")
    private String primaryAreaAttachmentTopicName;

    @Value("${esu.consumer.topic.job.execution.info.msg}")
    private String jobExecutionInfoMsgTopicName;

    @Value("${esu.service.address}")
    private String bootstrapServers;

    @Value("${esu.consumer.group.id}")
    private String consumerGroupId;

    @Value("${esu.consumer.topic.threads.number}")
    private Integer topicThreadsNumber;

    @Value("${esu.consumer.polling.interval}")
    private Integer pollingInterval;

    @Value("${esu.consumer.polling.timeout}")
    private Integer pollingTimeout;

    @Value("${esu.consumer.error.producer.timeout}")
    private Integer producerTimeout;

    @PostConstruct
    public void subscribeTopics() {
        //К_УУ_ЕСУ_1
        consumerJobExecutionInfoMsg = subscribeTopic(jobExecutionInfoMsgTopicName, esuConsumerDatabaseProcessor);
        //К_УУ_ЕСУ_3
        consumerPrimaryAreaAttachment = subscribeTopic(primaryAreaAttachmentTopicName, esuConsumerDatabaseProcessor);
    }

    private EsuTopicConsumer subscribeTopic(String topicName, EsuConsumerMessageProcessor processor) {
        EsuTopicConsumer consumer = null;

        try {
            consumer = buildConsumer(topicName, processor);
            //Подписываемся на топик
            consumer.consume();
        }
        catch (Exception ex) {
            LOG.error("Не удалось подписаться на ЕСУ топик " + topicName, ex);
        }
        return consumer;
    }

    private EsuTopicConsumer buildConsumer(String topicName, EsuConsumerMessageProcessor processor)
            throws ReflectionException, IntrospectionException, MalformedObjectNameException {
        EsuConsumer.EsuConsumerConfig consumerConfig = EsuConsumer.config(
                // Сервера для подключения к ЕСУ Kafka
                bootstrapServers,
                // Идентификатор подписчика
                consumerGroupId,
                // Название топика
                topicName,
                // Кол-во потоков
                topicThreadsNumber,
                // Объект класса, унаследованного от EsuConsumerMessageProcessor. NOT THREAD SAFE
                processor,
                // Важность очерёдности обработки сообщений (в случае если очерёдность важна, обработка будет выполняться в один поток и при наличии ошибок обработка сообщений будет останавливаться
                false);
        consumerConfig
                // Интервал между запросами в Kafka в мс (по-умолчанию 300)
                .setPollingInterval(pollingInterval)
                // Таймаут чтения сообщений из Kafka в мс (по-умолчанию 300)
                .setPollingTimeout(pollingTimeout)
                // Таймаут продюсера, при отправке сообщений в топик ConsumerErrors
                .setProducerTimeout(producerTimeout);
        return consumerConfig.build();
    }
}
