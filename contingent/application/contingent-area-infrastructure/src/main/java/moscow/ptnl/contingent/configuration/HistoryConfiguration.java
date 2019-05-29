/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.contingent.configuration;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.integration.annotation.IntegrationComponentScan;
import org.springframework.integration.channel.QueueChannel;
import org.springframework.integration.config.EnableIntegration;
import org.springframework.integration.scheduling.PollerMetadata;
import org.springframework.messaging.MessageChannel;
import org.springframework.scheduling.support.PeriodicTrigger;

/**
 * Для использования:
 * <pre>{@code
 * @Autowired
 * private MessageChannel historyChannel;
 * ...
 * historyChannel.send(
 *          HistoryEventBuilder
 *              .withTableAndObject(JournalHistoryTable.AREA, 1L)
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
public class HistoryConfiguration {
    
    private static final Logger LOG = LoggerFactory.getLogger(HistoryConfiguration.class);
    
    public static final String HISTORY_EVENT_CHANNEL_NAME = "HistoryEventChannel";
    
    
    /**
     * Канал для отправки сообщений в историю.   
     * 
     * @return 
     */
    @Bean(name = HISTORY_EVENT_CHANNEL_NAME)
    public MessageChannel createHistoryEventChannel() {        
        //return new DirectChannel();
        return new QueueChannel(10);
    }
    
    @Bean(name = PollerMetadata.DEFAULT_POLLER_METADATA_BEAN_NAME)
    public PollerMetadata defaultPoller() {
        PollerMetadata pollerMetadata = new PollerMetadata();
        pollerMetadata.setTrigger(new PeriodicTrigger(5));
        return pollerMetadata;
    }
    
}
