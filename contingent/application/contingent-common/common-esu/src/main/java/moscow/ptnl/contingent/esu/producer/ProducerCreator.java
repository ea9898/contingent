package moscow.ptnl.contingent.esu.producer;

import org.apache.kafka.common.security.plain.PlainLoginModule;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import ru.mos.emias.esu.lib.AuthorizationProps;
import ru.mos.emias.esu.lib.producer.EsuProducer;
import ru.mos.emias.esu.lib.producer.EsuProducerBuilder;
import ru.mos.emias.esu.lib.property.EsuLogProducerPropertiesBuilder;

/**
 *
 * @author m.kachalov
 */
public class ProducerCreator {
    
    private static final Logger LOG = LoggerFactory.getLogger(ProducerCreator.class);
    
    private static final int LINGER_MS = 1;
    
    private final EsuProducerBuilder builder;
    private String metricMessageProduct;
    private String logServers;
    
    /**
     * 
     * @param bootstrapServers
     * @param producerId
     * @param requestTimeoutMs "request.timeout.ms"
     */
    private ProducerCreator(String bootstrapServers, String producerId, int requestTimeoutMs, Integer maxRequestSize) {
        if (requestTimeoutMs < 20000) {
            LOG.warn("request.timeout.ms имеет значение [" + requestTimeoutMs + "], что меньше рекомендованного 60000 мс и меньше минимально возможного 20000 мс");
        }
        
        this.builder = new EsuProducerBuilder(bootstrapServers, producerId, requestTimeoutMs);
        this.builder
                .withMaxRequestSize(maxRequestSize)
                .withCustomProperty("enable.idempotence", false)
                //An upper bound on the time to report success or failure after a call to send() returns.
                //Задается в соответствии с формулой (request.timeout.ms + linger.ms) * retries
                .withCustomProperty("delivery.timeout.ms", requestTimeoutMs + LINGER_MS)
                .withCustomProperty("max.block.ms", 6000)
                //The configuration controls how long the KafkaProducer's send(), partitionsFor(), initTransactions(),
                // sendOffsetsToTransaction(), commitTransaction() and abortTransaction() methods will block.
                .withCustomProperty("linger.ms", LINGER_MS);
    }
    
    public static ProducerCreator create(String bootstrapServers, String producerId, int timeoutMs, Integer maxRequestSize) {
        ProducerCreator creator = new ProducerCreator(bootstrapServers, producerId, timeoutMs, maxRequestSize);
        return creator;
    }
    
    public static ProducerCreator create(ProducerProperties properties) {
        ProducerCreator creator = new ProducerCreator(
                properties.getBootstrapServers(), 
                properties.getProducerId(), 
                properties.getDeliveryTimeout(),
                properties.getMaxRequestSize()
        );
        creator.logServers = properties.getLogServers();
        creator.metricMessageProduct = properties.getLogProducerId();
        return creator;
    }
    
    public ProducerCreator withAuthorizationPlain(String userName, String password) {
        AuthorizationProps authorizationProps = new AuthorizationProps();
        authorizationProps.setSaslMechanism("PLAIN");
        authorizationProps.setSecurityProtocol("SASL_PLAINTEXT");
        authorizationProps.setSslEnabledProtocols(null);
        authorizationProps.setJaasCfg(PlainLoginModule.class.getName() +
                " required username=\"" + userName +
                "\" password=\"" + password + "\";");
        this.builder.withAuthorizationProps(authorizationProps);
        return this;
    }
    
    public ProducerCreator withAuthorizationProps(AuthorizationProps authorizationProps) {
        this.builder.withAuthorizationProps(authorizationProps);
        return this;
    }
    
    public ProducerCreator metricMessageProduct(String metricMessageProduct) {
        this.metricMessageProduct = metricMessageProduct;
        return this;
    }
    
    public ProducerCreator logServers(String logServers) {
        this.logServers = logServers;
        return this;
    }
    
    public EsuProducer build(boolean logEnabled) {
        if (logEnabled) {
            if (metricMessageProduct == null) {
                throw new RuntimeException("Не указано свойство metricMessageProduct");
            }
            if (logServers == null) {
                throw new RuntimeException("Не указано свойство logServers");
            }
            builder.withCustomLogProducerProperties( //логирование продюсера отправкой метрик в специальные топики Kafka
                new EsuLogProducerPropertiesBuilder(logServers, metricMessageProduct)
                    .enabled(logEnabled)
                    .withProduct(metricMessageProduct)
                    .build()
            );
        } else {
           builder.withCustomLogProducerProperties( //логирование продюсера отправкой метрик в специальные топики Kafka
                new EsuLogProducerPropertiesBuilder(
                        logServers != null ? logServers : "localhost:9092", 
                        metricMessageProduct != null ? metricMessageProduct : "UNKNOWN")
                    .enabled(logEnabled)
                    .build()
            ); 
        }
        return builder.build();
    }
    
}
