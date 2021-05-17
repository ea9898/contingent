package moscow.ptnl.contingent.area.configuration;

import moscow.ptnl.contingent.area.batch.CleanableJobRepository;
import moscow.ptnl.contingent.area.batch.CleanableMapJobRepositoryFactoryBean;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.JobParametersInvalidException;
import org.springframework.batch.core.configuration.BatchConfigurationException;
import org.springframework.batch.core.configuration.annotation.BatchConfigurer;
import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.batch.core.configuration.annotation.JobBuilderFactory;
import org.springframework.batch.core.configuration.annotation.StepBuilderFactory;
import org.springframework.batch.core.explore.JobExplorer;
import org.springframework.batch.core.explore.support.MapJobExplorerFactoryBean;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.batch.core.launch.support.RunIdIncrementer;
import org.springframework.batch.core.launch.support.SimpleJobLauncher;
import org.springframework.batch.core.repository.JobExecutionAlreadyRunningException;
import org.springframework.batch.core.repository.JobInstanceAlreadyCompleteException;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.repository.JobRestartException;
import org.springframework.batch.core.repository.support.MapJobRepositoryFactoryBean;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.support.transaction.ResourcelessTransactionManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;
import org.springframework.core.task.SimpleAsyncTaskExecutor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.transaction.PlatformTransactionManager;

import java.util.Set;
import java.util.UUID;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.PostConstruct;

@Configuration
@EnableBatchProcessing
public class BatchConfig implements BatchConfigurer {
    
    private static final Logger LOG = LoggerFactory.getLogger(BatchConfig.class);

    @Autowired
    @Lazy
    private JobBuilderFactory jobs;

    @Autowired
    @Lazy
    private StepBuilderFactory steps;

    @Autowired
    @Qualifier("attachmentPrimaryTopicTask")
    private Tasklet attachmentPrimaryTopicTask;

    @Autowired
    @Qualifier("jobExecutionInfoMsgTopicTask")
    private Tasklet jobExecutionInfoMsgTopicTask;

    @Autowired
    @Qualifier("dnEventInformerTask")
    private Tasklet dnEventInformerTask;

    @Autowired
    @Qualifier("cleanerTask")
    private Tasklet cleanerTask;

    private PlatformTransactionManager transactionManager;

    private JobRepository jobRepository;

    private JobLauncher jobLauncher;

    private JobExplorer jobExplorer;

    private MapJobRepositoryFactoryBean mapJobRepositoryFactoryBean;

    @PostConstruct
    public void initialize() {
        try {
            this.transactionManager = new ResourcelessTransactionManager();
            this.jobRepository = this.createJobRepository();
            this.jobExplorer = this.createJobExplorer();
            this.jobLauncher = this.createJobLauncher();
        } catch (Exception var3) {
            throw new BatchConfigurationException(var3);
        }
    }

    @Bean
    @Lazy
    public MapJobRepositoryFactoryBean getMapJobRepositoryFactoryBean() {
        return mapJobRepositoryFactoryBean;
    }

    protected JobRepository createJobRepository() {
        try {
            mapJobRepositoryFactoryBean = new CleanableMapJobRepositoryFactoryBean();
            mapJobRepositoryFactoryBean.afterPropertiesSet();
            return mapJobRepositoryFactoryBean.getObject();
        } catch (Exception ex) {
            throw new IllegalStateException("Can't initialize Batch Job Repository", ex);
        }
    }

    protected JobExplorer createJobExplorer() throws Exception {
        MapJobExplorerFactoryBean jobExplorerFactory = new MapJobExplorerFactoryBean(mapJobRepositoryFactoryBean);
        return jobExplorerFactory.getObject();
    }

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

    @Scheduled(fixedDelay = 10000, initialDelay = 40000)
    public void scheduleCleaner() {
        try {
            boolean jobsActive = jobExplorer.getJobNames().stream()
                    .distinct()
                    .anyMatch(this::isJobRunning);

            if (!jobsActive) {
                ((CleanableJobRepository) mapJobRepositoryFactoryBean.getJobRepository()).clean();
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
        getJobLauncher().run(job, params);
    }

    private boolean isJobRunning(String jobName) {
        Set<JobExecution> jobExecutions = getJobExplorer().findRunningJobExecutions(jobName);
        return !jobExecutions.isEmpty();
    }

    @Override
    public JobRepository getJobRepository() {
        return this.jobRepository;
    }

    @Override
    public PlatformTransactionManager getTransactionManager() {
        return this.transactionManager;
    }

    @Override
    public JobLauncher getJobLauncher() {
        return this.jobLauncher;
    }

    @Override
    public JobExplorer getJobExplorer() {
        return this.jobExplorer;
    }
}
