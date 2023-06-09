package moscow.ptnl.contingent.esu;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Component;

/**
 *
 * @author sorlov
 */
@Component
@PropertySource("classpath:application-esu.properties")
public class EsuProperties {
    
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

    @Value("${esu.service.address}")
    private String esuServers;
    
    @Value("${esu.log.producer.enabled:true}")
    private Boolean logEnabled; //разрешить логирование работы в специальный топик Kafka
    
    @Value("${esu.log.producer.servers:}") 
    private String logServers; //сервера куда отправляются лог сообщения
    
    @Value("${esu.log.producer.metric.message.product:}")
    private String metricMessageProduct; //идентификатор продукта для логирования

    public String getConsumerGroupId() {
        return consumerGroupId;
    }

    public Integer getTopicThreadsNumber() {
        return topicThreadsNumber;
    }

    public Integer getPollingInterval() {
        return pollingInterval;
    }

    public Integer getPollingTimeout() {
        return pollingTimeout;
    }

    public Integer getProducerTimeout() {
        return producerTimeout;
    }

    public String getBootstrapServersESU() { return esuServers; }
    
    public boolean isLogEnabled() {
        return Boolean.TRUE.equals(logEnabled);
    }
    
    public String getLogServers() {
        return logServers;
    }

    public String getMetricMessageProduct() {
        return metricMessageProduct;
    }
}
