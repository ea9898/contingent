package moscow.ptnl.contingent.configuration.nsi;

import static moscow.ptnl.contingent.configuration.nsi.Constraint.NSI_EVENT_CHANNEL_NAME;
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
 * 
 * 
 * @author mkachalov
 */
@Configuration
@EnableIntegration
@IntegrationComponentScan
public class EventChannelsConfiguration {
    
    private static final Logger LOG = LoggerFactory.getLogger(EventChannelsConfiguration.class);
    
    private static final int QUEUE_LENGTH = 1000;
    private static final int QUEUE_DELIVERY_INTERVAL = 5; //msec 
      
    
    
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

    
    //FIXME временно убрано до разделения с area-infrastructure
    /*
    @Bean(name = PollerMetadata.DEFAULT_POLLER_METADATA_BEAN_NAME)
    public PollerMetadata defaultPoller() {
        PollerMetadata pollerMetadata = new PollerMetadata();
        pollerMetadata.setTrigger(new PeriodicTrigger(QUEUE_DELIVERY_INTERVAL));
        return pollerMetadata;
    }
    */
    
}
