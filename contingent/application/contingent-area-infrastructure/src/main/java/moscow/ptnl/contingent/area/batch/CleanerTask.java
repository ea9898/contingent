package moscow.ptnl.contingent.area.batch;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.explore.JobExplorer;
import org.springframework.batch.core.repository.support.MapJobRepositoryFactoryBean;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import java.util.Objects;

/**
 * @author sorlov
 */
@Component
@Qualifier("cleanerTask")
public class CleanerTask implements Tasklet {

    private static final Logger LOG = LoggerFactory.getLogger(CleanerTask.class);

    @Autowired
    @Lazy
    private JobExplorer jobExplorer;

    @Autowired
    @Lazy
    private MapJobRepositoryFactoryBean mapJobRepositoryFactoryBean;

    @Override
    public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) {
        String currentJobName = contribution.getStepExecution().getJobExecution().getJobInstance().getJobName();
        boolean noJobsActive = jobExplorer.getJobNames().stream()
                .distinct()
                .filter(j -> !Objects.equals(j, currentJobName))
                .anyMatch(j -> !jobExplorer.findRunningJobExecutions(j).isEmpty());

        if (noJobsActive) {
            try {
                ((CleanableJobRepository) mapJobRepositoryFactoryBean.getJobRepository()).clean();
            } catch (Exception ex) {
                LOG.warn("Can't clean Batch jobs", ex);
            }
        }
        return RepeatStatus.FINISHED;
    }
}
