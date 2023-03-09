package moscow.ptnl.contingent.area.configuration;

import moscow.ptnl.contingent.batch.BaseTasklet;
import moscow.ptnl.contingent.batch.BatchHelper;
import moscow.ptnl.contingent.esuinput.executor.DNEventInformerJsonTask;
import moscow.ptnl.contingent.scheduler.AreaInfoSendTrigger;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;
import org.springframework.context.annotation.PropertySource;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

/**
 *
 * @author m.kachalov
 */
@Configuration
@EnableScheduling
@PropertySource("classpath:application-esu-trigger.properties")
public class ScheduleConfiguration {
        
    @Autowired @Lazy
    private BatchHelper batchHelper;
    
//    @Autowired
//    @Qualifier(DNEventInformerJsonTask.TASK_NAME + BaseTasklet.SUFFIX)
//    private Tasklet dnEventInformerTask;
    
    @Autowired
    private AreaInfoSendTrigger areaInfoSendTrigger;
        
//    @Scheduled(fixedDelay = 60000, initialDelay = 30000) Оставлено в качестве примера реализации планировщика
//    public void schedule3() {
//        batchHelper.schedule(dnEventInformerTask, "jobDnEventInformer");
//    }

    @Scheduled(fixedDelay = 600000, initialDelay = 40000)
    public void scheduleCleaner() {
        batchHelper.clean(7); //чистим старые записи в служебных таблицах SpringBatch  (время в днях)  
    }
    
    @Scheduled(cron = "${area-info.sync.k1.cron.rule}")
    public void scheduleAreaInfoSendTrigger() {
        areaInfoSendTrigger.schedule();
    }
    
}
