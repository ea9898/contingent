package moscow.ptnl.contingent.area.configuration;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.JobParametersInvalidException;
import org.springframework.batch.core.configuration.annotation.DefaultBatchConfigurer;
import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.batch.core.configuration.annotation.JobBuilderFactory;
import org.springframework.batch.core.configuration.annotation.StepBuilderFactory;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.batch.core.launch.support.RunIdIncrementer;
import org.springframework.batch.core.repository.JobExecutionAlreadyRunningException;
import org.springframework.batch.core.repository.JobInstanceAlreadyCompleteException;
import org.springframework.batch.core.repository.JobRestartException;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.Scheduled;
import javax.sql.DataSource;

@Configuration
@EnableBatchProcessing
public class BatchConfig extends DefaultBatchConfigurer {

    @Autowired
    private JobBuilderFactory jobs;

    @Autowired
    private StepBuilderFactory steps;

    @Autowired
    private JobLauncher jobLauncher;

    @Autowired
    @Qualifier("attachmentPrimaryTopicTask")
    private Tasklet attachmentPrimaryTopicTask;

    @Autowired
    @Qualifier("jobExecutionInfoMsgTopicTask")
    private Tasklet jobExecutionInfoMsgTopicTask;

    private Job buildJobAttachmentPrimaryTopicTask() {
        return jobs.get("jobAttachmentPrimaryTopicTask")
                .incrementer(new RunIdIncrementer())
                .start(steps.get("stepAttachmentPrimaryTopicTask")
                        .tasklet(attachmentPrimaryTopicTask)
                        .build())
                .build();
    }

    private Job buildJobJobExecutionInfoMsg() {
        return jobs.get("jobJobExecutionInfoMsg")
                .incrementer(new RunIdIncrementer())
                .start(steps.get("stepJobExecutionInfoMsg")
                        .tasklet(jobExecutionInfoMsgTopicTask)
                        .build())
                .build();
    }

    @Scheduled(fixedRate = 60000)
    public void schedule1() throws JobParametersInvalidException, JobExecutionAlreadyRunningException, JobRestartException, JobInstanceAlreadyCompleteException {
        JobParameters params = new JobParametersBuilder()
                .addString("JobID", String.valueOf(System.currentTimeMillis()))
                .toJobParameters();
        jobLauncher.run(buildJobAttachmentPrimaryTopicTask(), params);
    }

    @Scheduled(fixedRate = 60000)
    public void schedule2() throws JobParametersInvalidException, JobExecutionAlreadyRunningException, JobRestartException, JobInstanceAlreadyCompleteException {
        JobParameters params = new JobParametersBuilder()
                .addString("JobID", String.valueOf(System.currentTimeMillis()))
                .toJobParameters();
        jobLauncher.run(buildJobJobExecutionInfoMsg(), params);
    }

    @Override
    public void setDataSource(DataSource dataSource) {
        // override to do not set datasource even if a datasource exist.
        // initialize will use a Map based JobRepository (instead of database)
    }
}
