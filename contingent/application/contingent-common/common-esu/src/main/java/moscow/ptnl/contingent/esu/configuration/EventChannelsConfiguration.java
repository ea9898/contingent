package moscow.ptnl.contingent.esu.configuration;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskExecutor;
import org.springframework.integration.scheduling.PollerMetadata;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.scheduling.support.PeriodicTrigger;

/**
 *
 * @author mkachalov
 */
@Configuration("CommonEventChannelsConfiguration")
public class EventChannelsConfiguration {
    
    public static final int QUEUE_DELIVERY_INTERVAL = 50; //msec
    public static final int QUEUE_LENGTH = 1000;
    public static final String SI_TASK_EXECUTOR_NAME = "si_task_executor";
    
    @Bean(name = PollerMetadata.DEFAULT_POLLER)
    public PollerMetadata createDefaultPoller(@Qualifier(SI_TASK_EXECUTOR_NAME) TaskExecutor executor) {
        PollerMetadata pollerMetadata = new PollerMetadata();
        pollerMetadata.setTrigger(new PeriodicTrigger(QUEUE_DELIVERY_INTERVAL));
        pollerMetadata.setTaskExecutor(executor);
        return pollerMetadata;
    }
    
    @Bean(name = SI_TASK_EXECUTOR_NAME)
    public TaskExecutor createThreadPoolTaskExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(10);
        executor.setQueueCapacity(50);
        executor.setThreadNamePrefix("integration_task_executor_thread");
        executor.initialize();
        return executor;
    }
    
}
