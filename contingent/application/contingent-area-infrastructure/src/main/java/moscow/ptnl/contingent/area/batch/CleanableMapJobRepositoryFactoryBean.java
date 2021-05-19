package moscow.ptnl.contingent.area.batch;

import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.repository.dao.ExecutionContextDao;
import org.springframework.batch.core.repository.dao.JobExecutionDao;
import org.springframework.batch.core.repository.dao.JobInstanceDao;
import org.springframework.batch.core.repository.dao.MapExecutionContextDao;
import org.springframework.batch.core.repository.dao.MapJobExecutionDao;
import org.springframework.batch.core.repository.dao.MapJobInstanceDao;
import org.springframework.batch.core.repository.dao.MapStepExecutionDao;
import org.springframework.batch.core.repository.dao.StepExecutionDao;
import org.springframework.batch.core.repository.support.MapJobRepositoryFactoryBean;
import org.springframework.batch.core.repository.support.SimpleJobRepository;
import org.springframework.util.Assert;

/**
 * @author sorlov
 */
public class CleanableMapJobRepositoryFactoryBean extends MapJobRepositoryFactoryBean {

    private final CleanableSimpleJobRepository jobRepository;

    public CleanableMapJobRepositoryFactoryBean() throws Exception {
        jobRepository = new CleanableSimpleJobRepository(createJobInstanceDao(), createJobExecutionDao(), createStepExecutionDao(), createExecutionContextDao());
    }

    @Override
    public JobRepository getObject() throws Exception {
        return jobRepository;
    }

    @Override
    public void afterPropertiesSet() throws Exception {
    }

    public class CleanableSimpleJobRepository extends SimpleJobRepository implements CleanableJobRepository {

        CleanableSimpleJobRepository(final JobInstanceDao jobInstanceDao,
                                     final JobExecutionDao jobExecutionDao,
                                     final StepExecutionDao stepExecutionDao,
                                     final ExecutionContextDao ecDao) {
            super(jobInstanceDao, jobExecutionDao, stepExecutionDao, ecDao);
        }

        @Override
        public void clean() {
            ((MapJobInstanceDao) getJobInstanceDao()).clear();
            ((MapJobExecutionDao) getJobExecutionDao()).clear();
            ((MapStepExecutionDao) getStepExecutionDao()).clear();
            ((MapExecutionContextDao) getExecutionContextDao()).clear();
        }
    }
}
