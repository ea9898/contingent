package moscow.ptnl.contingent.area.configuration;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.JobParametersInvalidException;
import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.batch.core.launch.support.RunIdIncrementer;
import org.springframework.batch.core.repository.JobExecutionAlreadyRunningException;
import org.springframework.batch.core.repository.JobInstanceAlreadyCompleteException;
import org.springframework.batch.core.repository.JobRestartException;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.Scheduled;

import java.util.Set;
import java.util.UUID;
import javax.sql.DataSource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.support.DefaultBatchConfiguration;
import org.springframework.batch.core.explore.JobExplorer;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.context.annotation.Lazy;
import org.springframework.transaction.PlatformTransactionManager;

//@Configuration
//@EnableBatchProcessing
public class BatchConfig extends DefaultBatchConfiguration {
    
    private static final Logger LOG = LoggerFactory.getLogger(BatchConfig.class);

    @Autowired
    @Qualifier("attachmentPrimaryTopicTask")
    private Tasklet attachmentPrimaryTopicTask;

    @Autowired
    @Qualifier("jobExecutionInfoMsgTopicTask")
    private Tasklet jobExecutionInfoMsgTopicTask;

    @Autowired
    @Qualifier("dnEventInformerTask")
    private Tasklet dnEventInformerTask;
    
    @Autowired @Lazy
    private JobRepository jobRepository;
    
    @Autowired @Lazy
    private JobExplorer jobExplorer;
    
    @Autowired @Lazy
    private JobLauncher jobLauncher;
    
    @Autowired
    private DataSource dataSource;
    
    @Autowired
    private PlatformTransactionManager transactionManager;
    
    
    @Override
    public DataSource getDataSource() {
        return dataSource;
    }
    
    @Override
    public PlatformTransactionManager getTransactionManager() {
        return transactionManager;
    }
    
    private Job buildJob(Tasklet tasklet, String taskName) {
        Step step = new StepBuilder(taskName + "Step", jobRepository)
                .tasklet(tasklet, getTransactionManager())
                .build();
        return new JobBuilder(taskName, jobRepository)
                .incrementer(new RunIdIncrementer())
                .start(step)
                .build();
    }

    @Scheduled(fixedDelay = 60000, initialDelay = 10000)
    public void schedule1() {
        try {
            runJob(buildJob(attachmentPrimaryTopicTask, "jobAttachmentPrimaryTopicTask"));
        } catch (Exception e) {
            LOG.error("Ошибка в jobAttachmentPrimaryTopicTask", e);
        }
    }

    @Scheduled(fixedDelay = 60000, initialDelay = 20000)
    public void schedule2() {
        try {
            runJob(buildJob(jobExecutionInfoMsgTopicTask, "jobJobExecutionInfoMsg"));
        } catch (Exception e) {
            LOG.error("Ошибка в jobJobExecutionInfoMsg", e);
        }
    }

    @Scheduled(fixedDelay = 60000, initialDelay = 30000)
    public void schedule3() {
        try {
            runJob(buildJob(dnEventInformerTask, "jobDnEventInformer"));
        } catch (Exception e) {
            LOG.error("Ошибка в jobDnEventInformer", e);
        }
    }

    @Scheduled(fixedDelay = 10000, initialDelay = 40000)
    public void scheduleCleaner() {
        try {
            boolean jobsActive = jobExplorer.getJobNames().stream()
                    .distinct()
                    .anyMatch(this::isJobRunning);

            if (!jobsActive) {
                //((CleanableJobRepository) jobRepository).clean();
            }
        } catch (Exception e) {
            LOG.error("Ошибка в jobCleaner", e);
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

    private boolean isJobRunning(String jobName) {
        Set<JobExecution> jobExecutions = jobExplorer.findRunningJobExecutions(jobName);
        return !jobExecutions.isEmpty();
    }
}
