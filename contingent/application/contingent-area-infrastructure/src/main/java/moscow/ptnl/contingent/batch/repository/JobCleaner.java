package moscow.ptnl.contingent.batch.repository;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.Query;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Date;
import org.springframework.batch.core.repository.dao.AbstractJdbcBatchMetadataDao;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

/**
 * https://www.javatips.net/api/spring-batch-toolkit-master/src/main/java/com/javaetmoi/core/batch/tasklet/RemoveSpringBatchHistoryTasklet.java
    
 * @author m.kachalov
 */
@Repository
@Transactional(propagation = Propagation.REQUIRED)
public class JobCleaner {
    
    private static final String TABLE_PREFIX = AbstractJdbcBatchMetadataDao.DEFAULT_TABLE_PREFIX;
    
    private static final String[] QUERIES = new String[]{
        "DELETE FROM %PREFIX%STEP_EXECUTION_CONTEXT WHERE STEP_EXECUTION_ID IN (SELECT STEP_EXECUTION_ID FROM %PREFIX%STEP_EXECUTION WHERE JOB_EXECUTION_ID IN (SELECT JOB_EXECUTION_ID FROM  %PREFIX%JOB_EXECUTION where CREATE_TIME < ?1))",
        "DELETE FROM %PREFIX%STEP_EXECUTION WHERE JOB_EXECUTION_ID IN (SELECT JOB_EXECUTION_ID FROM %PREFIX%JOB_EXECUTION where CREATE_TIME < ?1)",
        "DELETE FROM %PREFIX%JOB_EXECUTION_CONTEXT WHERE JOB_EXECUTION_ID IN (SELECT JOB_EXECUTION_ID FROM  %PREFIX%JOB_EXECUTION where CREATE_TIME < ?1)",
        "DELETE FROM %PREFIX%JOB_EXECUTION_PARAMS WHERE JOB_EXECUTION_ID IN (SELECT JOB_EXECUTION_ID FROM %PREFIX%JOB_EXECUTION where CREATE_TIME < ?1)",
        "DELETE FROM %PREFIX%JOB_EXECUTION where CREATE_TIME < ?1",
        "DELETE FROM %PREFIX%JOB_INSTANCE WHERE JOB_INSTANCE_ID NOT IN (SELECT JOB_INSTANCE_ID FROM %PREFIX%JOB_EXECUTION)"
    };
    
    @PersistenceContext
    private EntityManager em;
    
    public void clean(int historicDepthInDays) {
        ZonedDateTime dt = Instant.now().atZone(ZoneId.of("Europe/Moscow")).minusDays(historicDepthInDays);
        Date date = Date.from(dt.toInstant());
        for (String sql : QUERIES) {
            if (sql.contains("?1")) {
                executeQuery(sql, date);
            } else {
                executeQuery(sql, null);
            }
        }
    }
    
    private int executeQuery(String sqlQuery, Date date) {
        String sql = StringUtils.replace(sqlQuery, "%PREFIX%", TABLE_PREFIX);
        Query query = em.createNativeQuery(sql);
        if (date != null) {
            query.setParameter(1, date);
        }
        return query.executeUpdate();
    }
    
}
