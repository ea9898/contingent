package moscow.ptnl.contingent.batch;

import java.util.Set;
import java.util.UUID;
import moscow.ptnl.contingent.batch.repository.JobCleaner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.JobParametersInvalidException;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.explore.JobExplorer;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.batch.core.launch.support.RunIdIncrementer;
import org.springframework.batch.core.repository.JobExecutionAlreadyRunningException;
import org.springframework.batch.core.repository.JobInstanceAlreadyCompleteException;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.repository.JobRestartException;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.PlatformTransactionManager;

/**
 *
 * @author m.kachalov
 */
@Component
public class BatchHelper {
    
    private static final Logger LOG = LoggerFactory.getLogger(BatchHelper.class);
    
    @Autowired
    private JobLauncher jobLauncher;
    
    @Autowired
    private JobExplorer jobExplorer;
    
    @Autowired
    private JobRepository jobRepository;
    
    @Autowired
    private PlatformTransactionManager transactionManager;
    
    @Autowired
    private JobCleaner jobCleaner;
    
    public void schedule(Tasklet task, String title) {
        try {
            runJob(buildJob(task, title));
        } catch (Exception e) {
            LOG.error("Ошибка в " + title, e);
        }
    }
    
    private void runJob(Job job) throws JobParametersInvalidException, JobExecutionAlreadyRunningException, JobRestartException, JobInstanceAlreadyCompleteException {
        if (job == null || isJobRunning(job.getName())) {
            return;
        }
        JobParameters params = new JobParametersBuilder()
                .addString("JobID", UUID.randomUUID().toString())
                .toJobParameters();
        jobLauncher.run(job, params);
    }
    
    private Job buildJob(Tasklet tasklet, String taskName) {
        Step step = new StepBuilder(taskName + "Step", jobRepository)
                .tasklet(tasklet, transactionManager)
                .build();
        return new JobBuilder(taskName, jobRepository)
                .incrementer(new RunIdIncrementer())
                .start(step)
                .build();
    }
    
    private boolean isJobRunning(String jobName) {
        Set<JobExecution> jobExecutions = jobExplorer.findRunningJobExecutions(jobName);
        return !jobExecutions.isEmpty();
    }
    
    /**
     * Удаляет завершенные задания из служебных таблиц SpringBatch в БД.
     * 
     * @param days интервал в днях после которого джоб считаем устаревшим
     */
    public void clean(int days) {
        try {
            boolean jobsActive = jobExplorer.getJobNames().stream()
                    .distinct()
                    .anyMatch(this::isJobRunning);

            if (!jobsActive) {
                jobCleaner.clean(days); //удаляем не активные джобы старше чем указано
            }
        } catch (Exception e) {
            LOG.error("Ошибка в jobCleaner", e);
        }
    }
    
}
