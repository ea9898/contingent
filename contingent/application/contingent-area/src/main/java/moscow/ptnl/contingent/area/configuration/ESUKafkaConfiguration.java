package moscow.ptnl.contingent.area.configuration;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.apache.kafka.common.serialization.StringSerializer;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.kafka.annotation.EnableKafka;
import org.springframework.kafka.core.ConsumerFactory;
import org.springframework.kafka.core.DefaultKafkaConsumerFactory;
import org.springframework.kafka.listener.ConcurrentMessageListenerContainer;
import org.springframework.kafka.config.ConcurrentKafkaListenerContainerFactory;
import org.springframework.kafka.config.KafkaListenerContainerFactory;
import org.springframework.kafka.core.DefaultKafkaProducerFactory;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.kafka.core.ProducerFactory;
import org.springframework.kafka.listener.ContainerProperties;
import ru.mos.emias.esu.lib.producer.EsuProducer;
import ru.mos.emias.esu.lib.utils.UtilityProducer;

/**
 * Конфигурационный файл для работы с Kafka для публикации в ЕСУ.
 *
 * @author m.kachalov
 */
@Configuration
@EnableKafka
@PropertySource("classpath:esu.properties")
public class ESUKafkaConfiguration {

    @Value("${esu.consuming.threads.number}")
    private Integer consumingThreadsNumber;
    @Value("${esu.consumer.id}")
    private String consumerGroupId;
    @Value("${esu.servers}")
    private String bootstrapServers;
    @Value("${esu.consumer.poll.timeout}")
    private Integer consumerPollTimeout;
    @Value("${esu.consumer.poll.max.records}")
    private Integer consumerMaxPollRecords;
    @Value("${esu.consumer.session.timeout}")
    private Integer consumerSessionTimeout;
    @Value("${esu.producer.produce.timeout}")
    private Integer producerRequestTimeout;

    @Value("${esu.servers}")
    private String esuServers;
    @Value("${esu.producer.id}")
    private String esuProducer;
    @Value("${esu.producer.error.send.timeout}")
    private int esuErrorSendTimeout;
    @Value("${esu.producer.error.retries}")
    private int esuErrorRetries;

    @Bean
    public EsuProducer esuProducer() {
        return new EsuProducer(esuServers, esuProducer, esuErrorSendTimeout, Optional.of(esuErrorRetries));
    }

    /**
     * Producer config
     */
    @Bean
    public Map<String, Object> producerConfigs() {
        Map<String, Object> props = new HashMap<>();
        // Bootstrap server list, separated by commas [,]
        props.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServers);
        // Key serializer class. Default - StringSerializer.class
        props.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class);
        // Message body serializer class. Default - StringSerializer.class
        props.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, StringSerializer.class);
        // Producer timeout
        props.put(ProducerConfig.REQUEST_TIMEOUT_MS_CONFIG, producerRequestTimeout);

        return props;
    }

    @Bean
    public ProducerFactory<String, String> producerFactory() {
        return new DefaultKafkaProducerFactory<>(producerConfigs());
    }

    @Bean
    public KafkaTemplate<String, String> kafkaTemplate() {
        return new KafkaTemplate<>(producerFactory());
    }

    @Bean
    public UtilityProducer producer() {
        return new UtilityProducer();
    }

    /**
     * Consumer config
     */
    @Bean
    public KafkaListenerContainerFactory<ConcurrentMessageListenerContainer<String, String>> kafkaListenerContainerFactory() {
        ConcurrentKafkaListenerContainerFactory<String, String> factory = new  ConcurrentKafkaListenerContainerFactory<>();
        factory.setConsumerFactory(consumerFactory());
        // Number of consumer threads
        factory.setConcurrency(consumingThreadsNumber);
        // Consumer polling timeout
        factory.getContainerProperties().setPollTimeout(consumerPollTimeout);
        // Acknowledge mode. I recommend always use MANUAL_IMMEDIATE mode.
        factory.getContainerProperties().setAckMode(ContainerProperties.AckMode.MANUAL_IMMEDIATE);
        // Acknowledge on error. I recommend always use false.
        factory.getContainerProperties().setAckOnError(false);
        return factory;
    }

    @Bean
    public ConsumerFactory<String, String> consumerFactory() {
        return new DefaultKafkaConsumerFactory<>(consumerConfigs());
    }

    @Bean
    public Map<String, Object> consumerConfigs() {
        Map<String, Object> props = new HashMap<>();
        // Bootstrap server list, separated by commas [,]
        props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServers);
        // Consumer group id. Must be equal on all consumer nodes.
        props.put(ConsumerConfig.GROUP_ID_CONFIG, consumerGroupId);
        // Auto commit (auto acknoledgment). Recomendation to use false.
        props.put(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, false);
        // Key (message identifier) serialization class. Default - StringDeserializer.class
        props.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class);
        // Message body serialization class. Default - StringDeserializer.class
        props.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class);
        /* Strategy of getting messages when polling the topic for first time.
        earliest - get all messages. latest - ignore contained messages */
        props.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest");
        // Consumer session timeout.
        props.put(ConsumerConfig.SESSION_TIMEOUT_MS_CONFIG, consumerSessionTimeout);
        // Maximum number of polling records at once.
        props.put(ConsumerConfig.MAX_POLL_RECORDS_CONFIG, consumerMaxPollRecords);

        return props;
    }

}