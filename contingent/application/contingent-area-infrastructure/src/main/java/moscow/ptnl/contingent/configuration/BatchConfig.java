package moscow.ptnl.contingent.configuration;

import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
@EnableBatchProcessing
public class BatchConfig {


    @Scheduled(fixedRate = 60000)
    public void schedule() {
        System.out.println("Task test... every 60 seconds");
    }

}
