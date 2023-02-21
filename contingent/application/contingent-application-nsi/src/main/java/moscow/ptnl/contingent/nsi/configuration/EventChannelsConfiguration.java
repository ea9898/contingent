package moscow.ptnl.contingent.nsi.configuration;

import static moscow.ptnl.contingent.esu.configuration.EventChannelsConfiguration.QUEUE_LENGTH;
import static moscow.ptnl.contingent.nsi.configuration.Constraint.*;

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
 * 
 * 
 * @author mkachalov
 */
@Configuration("NSIEventChannelsConfiguration")
@EnableIntegration
@IntegrationComponentScan("moscow.ptnl.contingent.nsi")
public class EventChannelsConfiguration {
    
    private static final Logger LOG = LoggerFactory.getLogger(EventChannelsConfiguration.class);

    /**
     * Канал для отправки сообщений в НСИ.
     *
     * @return
     */
    @Bean(name = NSI_EVENT_CHANNEL_NAME)
    public MessageChannel createNSIEventChannel() {
        //return new DirectChannel();
        return new QueueChannel(QUEUE_LENGTH);
    }

    /**
     * Канал для запроса адреса в НСИ.
     *
     * @return
     */
    @Bean(name = NSI_FORM_REQUEST_CHANNEL_NAME)
    public MessageChannel createNsiFormRequestChannel() {
        return new DirectChannel();
    }
    

    /**
     * Канал для сохранения адреса из НСИ.
     *
     * @return
     */
    @Bean(name = NSI_FORM_CHANNEL_NAME)
    public MessageChannel createNsiFormChannel() {
        return new DirectChannel();
    }
}
