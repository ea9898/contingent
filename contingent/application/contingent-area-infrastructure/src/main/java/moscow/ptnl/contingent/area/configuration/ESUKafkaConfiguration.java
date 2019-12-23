package moscow.ptnl.contingent.area.configuration;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import javax.annotation.PreDestroy;
import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.common.serialization.StringSerializer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;
import org.springframework.context.annotation.PropertySource;
import org.springframework.kafka.annotation.EnableKafka;
import org.springframework.kafka.core.DefaultKafkaProducerFactory;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.kafka.core.ProducerFactory;
import ru.mos.emias.esu.lib.producer.EsuProducer;

/**
 * Конфигурационный файл для работы с Kafka для публикации в ЕСУ.
 *
 * @author m.kachalov
 */
@Configuration
@EnableKafka
@PropertySource("classpath:application-esu.properties")
public class ESUKafkaConfiguration {
    
    private static final Logger LOG = LoggerFactory.getLogger(ESUKafkaConfiguration.class);

    @Value("${esu.service.address}")
    private String bootstrapServers;
    @Value("${esu.producer.produce.timeout}")
    private Integer producerRequestTimeout;
    @Value("${esu.service.address}")
    private String esuServers;
    @Value("${esu.producer.id}")
    private String esuProducer;
    @Value("${esu.producer.error.send.timeout}")
    private Integer esuErrorSendTimeout;
    @Value("${esu.producer.error.retries}")
    private Integer esuErrorRetries;
    
    /*
    @Bean
    public ESUEventPublishService getESUEventPublishService() {
        return new ESUEventPublishService();
    }
    
    @Bean
    public ESUOutputEventRepository getESUOutputEventRepository() {
        return new ESUOutputEventRepository();
    }
    
    @Bean
    public EventDataGeneratorRepository getEventDataGeneratorRepository() {
        return new EventDataGeneratorRepository();
    }
    */
    
    @Bean @Lazy
    public EsuProducer esuProducer() {
        return new EsuProducer(esuServers, esuProducer, esuErrorSendTimeout, Optional.of(esuErrorRetries), null){
            @PreDestroy
            public void preDestroy() {
                try {
                    close();
                } catch (Exception e) {
                    LOG.error("ошибка закрытия соединени продюсера ЕСУ", e);
                }
            }
        };
    }
   
    /**
     * Producer config
     */    
    private Map<String, Object> producerConfigs() {
        Map<String, Object> props = new HashMap<>();
        // Boostrap server list, separated by commas [,]
        props.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServers);
        // Key serializer class. Default - StringSerializer.class
        props.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class);
        // Message body serializer class. Default - StringSerializer.class
        props.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, StringSerializer.class);
        // Producer timeout
        props.put(ProducerConfig.REQUEST_TIMEOUT_MS_CONFIG, producerRequestTimeout);

        return props;
    }

    @Bean @Qualifier("esu")
    public ProducerFactory<String, String> kafkaProducerFactory() {
        return new DefaultKafkaProducerFactory<>(producerConfigs());
    }

    @Bean @Qualifier("esu")
    public KafkaTemplate<String, String> kafkaTemplate() {
        return new KafkaTemplate<>(kafkaProducerFactory());
    }

}
