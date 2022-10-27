package moscow.ptnl.contingent.area.configuration;

import java.util.HashMap;
import java.util.Map;

import moscow.ptnl.contingent.infrastructure.service.esu.producer.ProducerCreator;
import moscow.ptnl.contingent.infrastructure.service.esu.producer.ProducerProperties;

import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.common.serialization.StringSerializer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
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

    @Autowired
    private ProducerProperties producerProperties;

    @Bean(
            destroyMethod = "close"
    )
    public EsuProducer esuProducer() {
        return ProducerCreator.create(producerProperties)
                //.withAuthorizationProps(authorizationProps)
                //.withAuthorizationPlain(producerProperties.getMeshAuthUserName(), producerProperties.getMeshAuthPassword())
                .build(producerProperties.isLogEnabled());
    }

    /**
     * Producer config
     */    
    private Map<String, Object> producerConfigs() {
        Map<String, Object> props = new HashMap<>();
        // Boostrap server list, separated by commas [,]
        props.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, producerProperties.getBootstrapServers());
        // Key serializer class. Default - StringSerializer.class
        props.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class);
        // Message body serializer class. Default - StringSerializer.class
        props.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, StringSerializer.class);
        // Producer timeout
        props.put(ProducerConfig.REQUEST_TIMEOUT_MS_CONFIG, producerProperties.getDeliveryTimeout());

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
