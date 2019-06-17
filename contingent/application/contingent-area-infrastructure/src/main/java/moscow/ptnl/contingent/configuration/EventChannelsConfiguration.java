package moscow.ptnl.contingent.configuration;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.integration.annotation.IntegrationComponentScan;
import org.springframework.integration.channel.DirectChannel;
import org.springframework.integration.channel.QueueChannel;
import org.springframework.integration.config.EnableIntegration;
import org.springframework.integration.scheduling.PollerMetadata;
import org.springframework.messaging.MessageChannel;
import org.springframework.scheduling.support.PeriodicTrigger;

/**
 * Для использования:
 * <pre>{@code
 * @Autowired @Qualifier(EventChannelsConfiguration.HISTORY_EVENT_CHANNEL_NAME)
 * private MessageChannel historyChannel;
 * ...
 * historyChannel.send(
 *          HistoryEventBuilder
 *              .withEntity(entity.getClass(), entity.getId())
 *              .setPrincipal(UserContextHolder.getPrincipal())
 *              .addValue("field1", "old-val-1", "new-val-1")
 *              .addValue("field2", "old-val-2", "new-val-2")
 *              .buildMessage()
 *      );
 * }</pre>
 * 
 * @author mkachalov
 */
@Configuration
@EnableIntegration
@IntegrationComponentScan
public class EventChannelsConfiguration {
    
    private static final Logger LOG = LoggerFactory.getLogger(EventChannelsConfiguration.class);
    
    public static final String HISTORY_EVENT_CHANNEL_NAME = "HistoryEventChannel";
    public static final String ESU_EVENT_CHANNEL_NAME = "ESUEventChannel";
    
    private static final int QUEUE_LENGTH = 1000;
    private static final int QUEUE_DELIVERY_INTERVAL = 5; //msec 
        
    
    /**
     * Канал для отправки сообщений в историю.   
     * 
     * @return 
     */
    @Bean(name = HISTORY_EVENT_CHANNEL_NAME)
    public MessageChannel createHistoryEventChannel() {        
        //return new DirectChannel();
        return new QueueChannel(QUEUE_LENGTH);
    }
    
    /**
     * Канал для отправки сообщений в ЕСУ.   
     * 
     * @return 
     */
    @Bean(name = ESU_EVENT_CHANNEL_NAME)
    public MessageChannel createESUEventChannel() {        
        //return new DirectChannel();
        return new QueueChannel(QUEUE_LENGTH);
    }
    
    @Bean(name = PollerMetadata.DEFAULT_POLLER_METADATA_BEAN_NAME)
    public PollerMetadata defaultPoller() {
        PollerMetadata pollerMetadata = new PollerMetadata();
        pollerMetadata.setTrigger(new PeriodicTrigger(QUEUE_DELIVERY_INTERVAL));
        return pollerMetadata;
    }
    
}
