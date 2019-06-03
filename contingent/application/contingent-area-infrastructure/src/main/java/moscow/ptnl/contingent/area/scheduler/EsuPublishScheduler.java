package moscow.ptnl.contingent.area.scheduler;

import moscow.ptnl.contingent.service.EsuService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.task.SimpleAsyncTaskExecutor;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.scheduling.concurrent.ConcurrentTaskScheduler;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.lang.invoke.MethodHandles;
import java.time.LocalDateTime;
import java.util.concurrent.Executor;

@Configuration
@EnableScheduling
@PropertySource("classpath:application-esu.properties")
public class EsuPublishScheduler {

    private final static Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    @Autowired
    private EsuService esuService;

    @Value("${esu.resend.timeout}")
    private String esuResendTimeoutValue; //время сек, после которого можно повторно отправлять не отправленное сообщение

    @Bean
    public TaskScheduler taskScheduler() {
        return new ConcurrentTaskScheduler();
    }

    @Bean
    public Executor taskExecutor() {
        return new SimpleAsyncTaskExecutor();
    }

    /**
     * Периодическая проверка наличия неотправленных в ЕСУ сообщений и
     * попытка повторной их отправки.
     */
    @Transactional(propagation = Propagation.REQUIRED)
    @Scheduled(cron = "${esu.resend.publish.cron.rule}")
    public void esuTask() {
        LOG.debug("Start periodical task to resend messages to ESU");

        try {
            long esuResendTimeout = Long.parseLong(esuResendTimeoutValue);
            LocalDateTime date = LocalDateTime.now().minusSeconds(esuResendTimeout);
            esuService.periodicalPublishUnsuccessMessagesToESU(date);
        } catch (Exception e) {
            LOG.error("Ошибка выполнения периодического задания по переотправке сообщений в ЕСУ", e);
        }
    }
}
