package moscow.ptnl.contingent.area.configuration;

import static moscow.ptnl.contingent.configuration.EventChannelsConfiguration.QUEUE_LENGTH;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.integration.annotation.IntegrationComponentScan;
import org.springframework.integration.channel.DirectChannel;
import org.springframework.integration.channel.QueueChannel;
import org.springframework.integration.config.EnableIntegration;
import org.springframework.messaging.MessageChannel;

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
@Configuration("AreaEventChannelsConfiguration")
@EnableIntegration
@IntegrationComponentScan("moscow.ptnl.contingent.area")
public class EventChannelsConfiguration {
    
    private static final Logger LOG = LoggerFactory.getLogger(EventChannelsConfiguration.class);
    
    public static final String HISTORY_EVENT_CHANNEL_NAME = "HistoryEventChannel";
    public static final String ESU_EVENT_CHANNEL_NAME = "ESUEventChannel";
    public static final String SOAP_LOG_EVENT_CHANNEL_NAME = "LogSoapEventChannel";

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
     * Канал для логирования SOAP сообщений.
     *
     * @return
     */
    @Bean(name = SOAP_LOG_EVENT_CHANNEL_NAME)
    public MessageChannel createLogSoapEventChannel() {
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
    
    
}
