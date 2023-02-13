package moscow.ptnl.contingent.repository;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import moscow.ptnl.contingent.PersistenceConstraint;

public class BaseRepository {

    @PersistenceContext(unitName = PersistenceConstraint.PU_CONTINGENT_NAME)
    protected EntityManager entityManager;
    
    public EntityManager getEntityManager() {
        return entityManager;
    }

}
