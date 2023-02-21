package moscow.ptnl.contingent.batch.config;

import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Configuration;

import javax.sql.DataSource;
import moscow.ptnl.contingent.batch.BaseTasklet;
import moscow.ptnl.contingent.batch.BaseTopicExecutor;
import moscow.ptnl.contingent.esuinput.executor.AttachmentPrimaryTopicTask;
import moscow.ptnl.contingent.esuinput.executor.DNEventInformerJsonTask;
import moscow.ptnl.contingent.esuinput.executor.JobExecutionInfoMsgTopicTask;
import org.springframework.batch.core.configuration.support.DefaultBatchConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.transaction.PlatformTransactionManager;

@Configuration
@EnableBatchProcessing
public class BatchConfig extends DefaultBatchConfiguration {
        
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
    
    @Bean(JobExecutionInfoMsgTopicTask.TASK_NAME + BaseTasklet.SUFFIX)
    public BaseTasklet jobExecutionInfoMsgTopicTasklet(@Qualifier(JobExecutionInfoMsgTopicTask.TASK_NAME) BaseTopicExecutor executor) {
        return new BaseTasklet(executor);
    }

    @Bean(AttachmentPrimaryTopicTask.TASK_NAME + BaseTasklet.SUFFIX)
    public BaseTasklet attachmentPrimaryTopicTasklet(@Qualifier(AttachmentPrimaryTopicTask.TASK_NAME) BaseTopicExecutor executor) {
        return new BaseTasklet(executor);
    }
    
    @Bean(DNEventInformerJsonTask.TASK_NAME + BaseTasklet.SUFFIX)
    public BaseTasklet dnEventInformerTasklet(@Qualifier(DNEventInformerJsonTask.TASK_NAME) BaseTopicExecutor executor) {
        return new BaseTasklet(executor);
    }
}
