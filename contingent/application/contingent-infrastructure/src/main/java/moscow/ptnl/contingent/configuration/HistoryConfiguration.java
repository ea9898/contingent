/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.contingent.configuration;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.integration.annotation.ServiceActivator;
import org.springframework.integration.channel.QueueChannel;
import org.springframework.integration.config.EnableIntegration;
import org.springframework.messaging.Message;
import org.springframework.messaging.MessageChannel;

/**
 *
 * @author mkachalov
 */
@Configuration
@EnableIntegration
public class HistoryConfiguration {
    
    public static final String HISTORY_EVENT_CHANNEL_NAME = "HistoryEventChannel";
    
    @Bean(name = HISTORY_EVENT_CHANNEL_NAME)
    public MessageChannel createHistoryEventChannel() {
        return new QueueChannel();
    }
    
    @ServiceActivator(inputChannel = HISTORY_EVENT_CHANNEL_NAME)
    public void historyEventConsumer(Message<String> msg) {
        
    }
    
}
