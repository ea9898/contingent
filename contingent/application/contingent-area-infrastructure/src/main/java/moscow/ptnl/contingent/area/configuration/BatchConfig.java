package moscow.ptnl.contingent.area.configuration;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.JobParametersInvalidException;
import org.springframework.batch.core.configuration.annotation.DefaultBatchConfigurer;
import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.batch.core.configuration.annotation.JobBuilderFactory;
import org.springframework.batch.core.configuration.annotation.StepBuilderFactory;
import org.springframework.batch.core.explore.JobExplorer;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.batch.core.launch.support.RunIdIncrementer;
import org.springframework.batch.core.launch.support.SimpleJobLauncher;
import org.springframework.batch.core.repository.JobExecutionAlreadyRunningException;
import org.springframework.batch.core.repository.JobInstanceAlreadyCompleteException;
import org.springframework.batch.core.repository.JobRestartException;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.SimpleAsyncTaskExecutor;
import org.springframework.scheduling.annotation.Scheduled;
import javax.sql.DataSource;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Configuration
@EnableBatchProcessing
public class BatchConfig extends DefaultBatchConfigurer {
    
    private static final Logger LOG = LoggerFactory.getLogger(BatchConfig.class);

    @Autowired
    private JobBuilderFactory jobs;

    @Autowired
    private StepBuilderFactory steps;

    @Autowired
    private JobLauncher jobLauncher;

    @Autowired
    private JobExplorer jobExplorer;

    @Autowired
    @Qualifier("attachmentPrimaryTopicTask")
    private Tasklet attachmentPrimaryTopicTask;

    @Autowired
    @Qualifier("jobExecutionInfoMsgTopicTask")
    private Tasklet jobExecutionInfoMsgTopicTask;

    @Autowired
    @Qualifier("dnEventInformerTask")
    private Tasklet dnEventInformerTask;

    @Override
    protected JobLauncher createJobLauncher() throws Exception {
        SimpleAsyncTaskExecutor executor = new SimpleAsyncTaskExecutor("batch-task-thread-");
        executor.setConcurrencyLimit(10);
        SimpleJobLauncher launcher = new SimpleJobLauncher();
        launcher.setJobRepository(getJobRepository());
        launcher.setTaskExecutor(executor);
        launcher.afterPropertiesSet();
        return launcher;
    }
    
    private Job buildJob(Tasklet tasklet, String taskName) {
        return jobs.get(taskName)
                .incrementer(new RunIdIncrementer())
                .start(
                    steps.get(taskName + "Step")
                    .tasklet(tasklet)
                    .build()
                ).build();
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

    @Override
    public void setDataSource(DataSource dataSource) {
        // override to do not set datasource even if a datasource exist.
        // initialize will use a Map based JobRepository (instead of database)
    }

    private void runJob(Job job) throws JobParametersInvalidException, JobExecutionAlreadyRunningException, JobRestartException, JobInstanceAlreadyCompleteException {
        if (job == null || isJobRunning(job.getName())) {
            return;
        }
        //С этим течет память!
        //JobParameters params = new JobParametersBuilder()
        //        .addString("JobID", String.valueOf(System.currentTimeMillis()))
        //        .toJobParameters();
        jobLauncher.run(job, new JobParameters());
    }

    private boolean isJobRunning(String jobName) {
        Set<JobExecution> jobExecutions = jobExplorer.findRunningJobExecutions(jobName);
        return !jobExecutions.isEmpty();
    }
}
