package moscow.ptnl.contingent.esu.producer;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Component;

/**
 *
 * @author m.kachalov
 */
@Component
@PropertySource("classpath:application-esu.properties")
public class ProducerPropertiesImpl implements ProducerProperties {
    
    @Value("${esu.service.address}")
    private String bootstrapServers;

    @Value("${esu.producer.id}")
    private String producerId;

    @Value("${esu.producer.produce.timeout:5000}")
    private Integer sendTimeout;
    
    @Value("${esu.log.producer.enabled:true}")
    private Boolean logEnabled; //разрешить логирование работы в специальный топик Kafka
        
    @Value("${esu.log.producer.servers:}") 
    private String logServers; //сервера куда отправляются лог сообщения
    
    @Value("${esu.log.producer.metric.message.product:}")
    private String metricMessageProduct; //идентификатор продукта для логирования

    @Value("${esu.max.request.size}")
    private Integer maxRequestSize;

    @Override
    public String getBootstrapServers() {
        return bootstrapServers;
    }

    @Override
    public String getProducerId() {
        return producerId;
    }

    @Override
    public Integer getDeliveryTimeout() {
        return sendTimeout;
    }

    @Override
    public boolean isLogEnabled() {
        return Boolean.TRUE.equals(logEnabled);
    }

    @Override
    public String getLogServers() {
        return logServers;
    }

    @Override
    public String getLogProducerId() {
        return metricMessageProduct;
    }

    @Override
    public Integer getMaxRequestSize() { return maxRequestSize; }
}
