package moscow.ptnl.contingent.batch;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.Objects;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;

/**
 *
 * @author m.kachalov
 */
public class BaseTasklet implements Tasklet {
    
    public static final String SUFFIX = "_task";
    
    private static final Logger LOG = LoggerFactory.getLogger(BaseTasklet.class);
    
    private final BaseTopicExecutor executor;
    
    public BaseTasklet(BaseTopicExecutor executor) {
        Objects.requireNonNull(executor);
        this.executor = executor;
    }

    @Override
    public RepeatStatus execute(StepContribution sc, ChunkContext cc) throws Exception {
        try {
            LocalDateTime time = LocalDateTime.now();
            LOG.info(executor.getBeanName() + " task start");
            executor.execute();
            LOG.info(executor.getBeanName() + " task done in " + Duration.between(time, LocalDateTime.now()));
        } catch (Exception e) {
            LOG.error("Ошибка выполнения джоба", e);
        }
        return RepeatStatus.FINISHED;
    }
    
}
