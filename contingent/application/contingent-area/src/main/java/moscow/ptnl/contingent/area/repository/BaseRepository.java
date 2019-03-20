package moscow.ptnl.contingent.area.repository;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import moscow.ptnl.contingent.area.configuration.PersistenceConfiguration;

public class BaseRepository {

    @PersistenceContext(unitName = PersistenceConfiguration.PU_CONTINGENT_NAME)
    protected EntityManager entityManager;

}
