package moscow.ptnl.mod.health.dao;

import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import jakarta.persistence.Query;
import java.util.List;

/**
 * @author sorlov
 */
@Component
@Transactional(propagation = Propagation.MANDATORY)
public class DatabaseCheckDao extends BaseRepository {

    private static final Integer TIMEOUT = 10000;

    @SuppressWarnings("unchecked")
    public List<String> listAllTables() {
        Query query = getEntityManager().createNativeQuery("SELECT table_name\n" +
                "FROM information_schema.tables\n" +
                "WHERE NOT table_schema IN ('public', 'information_schema', 'pg_catalog')\n" +
                "ORDER BY table_name"
        );
        query.setHint("jakarta.persistence.query.timeout", TIMEOUT);
        query.setHint("jakarta.persistence.lock.timeout", TIMEOUT);

        return query.getResultList();
    }

    public long selectCountFromTable(String tableName) {
        Query query = getEntityManager().createNativeQuery("SELECT COUNT(*) FROM " + tableName);
        query.setHint("jakarta.persistence.query.timeout", TIMEOUT);
        query.setHint("jakarta.persistence.lock.timeout", TIMEOUT);

        return (Long) query.getSingleResult();
    }
}
