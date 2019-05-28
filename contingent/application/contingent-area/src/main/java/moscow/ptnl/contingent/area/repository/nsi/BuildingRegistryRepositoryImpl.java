package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.BuildingRegistry;
import moscow.ptnl.contingent.area.entity.nsi.BuildingRegistry_;
import moscow.ptnl.contingent.area.repository.BaseRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class BuildingRegistryRepositoryImpl extends BaseRepository implements BuildingRegistryRepository {

    @Override
    public List<BuildingRegistry> getBuildingsRegistry(long globalId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<BuildingRegistry> criteria = criteriaBuilder.createQuery(BuildingRegistry.class);
        Root<BuildingRegistry> template = criteria.from(BuildingRegistry.class);
        criteria.where(
                criteriaBuilder.and(
                        criteriaBuilder.equal(template.get(BuildingRegistry_.globalId.getName()), globalId)
                )
        );
        return entityManager.createQuery(criteria).getResultList();
    }

    @Override
    public List<BuildingRegistry> findRegistryBuildings(String l1Value, String l2Value, String l3Value, long addrId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<BuildingRegistry> criteria = criteriaBuilder.createQuery(BuildingRegistry.class);
        Root<BuildingRegistry> template = criteria.from(BuildingRegistry.class);
        criteria.where(
                criteriaBuilder.and(
                        criteriaBuilder.equal(template.get(BuildingRegistry_.l1Value.getName()), l1Value),
                        criteriaBuilder.equal(template.get(BuildingRegistry_.l2Value.getName()), l2Value),
                        criteriaBuilder.equal(template.get(BuildingRegistry_.l3Value.getName()), l3Value),
                        criteriaBuilder.equal(template.get(BuildingRegistry_.addrId.getName()), addrId)
                )
        );
        return entityManager.createQuery(criteria).getResultList();
    }
}
