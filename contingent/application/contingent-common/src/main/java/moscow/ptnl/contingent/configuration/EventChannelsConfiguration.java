package moscow.ptnl.contingent.configuration;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.integration.config.EnableIntegration;
import org.springframework.integration.scheduling.PollerMetadata;
import org.springframework.scheduling.support.PeriodicTrigger;

/**
 *
 * @author mkachalov
 */
@Configuration("CommonEventChannelsConfiguration")
@EnableIntegration
public class EventChannelsConfiguration {
    
    public static final int QUEUE_DELIVERY_INTERVAL = 5; //msec
    public static final int QUEUE_LENGTH = 1000;
    
    @Bean(name = PollerMetadata.DEFAULT_POLLER_METADATA_BEAN_NAME)
    public PollerMetadata defaultPoller() {
        PollerMetadata pollerMetadata = new PollerMetadata();
        pollerMetadata.setTrigger(new PeriodicTrigger(QUEUE_DELIVERY_INTERVAL));
        return pollerMetadata;
    }
    
}
