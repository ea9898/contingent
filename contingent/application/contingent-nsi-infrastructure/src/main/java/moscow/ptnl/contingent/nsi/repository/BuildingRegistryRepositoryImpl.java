package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import java.util.List;
import moscow.ptnl.contingent.nsi.domain.area.NsiBuildingRegistry;
import moscow.ptnl.contingent.nsi.domain.area.NsiBuildingRegistry_;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class BuildingRegistryRepositoryImpl extends BaseRepository implements BuildingRegistryRepository {

    @Override
    public List<NsiBuildingRegistry> getBuildingsRegistry(long globalId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<NsiBuildingRegistry> criteria = criteriaBuilder.createQuery(NsiBuildingRegistry.class);
        Root<NsiBuildingRegistry> template = criteria.from(NsiBuildingRegistry.class);
        criteria.where(
                criteriaBuilder.and(
                        criteriaBuilder.equal(template.get(NsiBuildingRegistry_.globalId.getName()), globalId)
                )
        );
        return entityManager.createQuery(criteria).getResultList();
    }

    @Override
    public List<NsiBuildingRegistry> findRegistryBuildings(String l1Value, String l2Value, String l3Value, long addrId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<NsiBuildingRegistry> criteria = criteriaBuilder.createQuery(NsiBuildingRegistry.class);
        Root<NsiBuildingRegistry> template = criteria.from(NsiBuildingRegistry.class);
        criteria.where(
                criteriaBuilder.and(
                        criteriaBuilder.equal(template.get(NsiBuildingRegistry_.l1Value.getName()), l1Value),
                        criteriaBuilder.equal(template.get(NsiBuildingRegistry_.l2Value.getName()), l2Value),
                        criteriaBuilder.equal(template.get(NsiBuildingRegistry_.l3Value.getName()), l3Value),
                        criteriaBuilder.equal(template.get(NsiBuildingRegistry_.addrId.getName()), addrId)
                )
        );
        return entityManager.createQuery(criteria).getResultList();
    }
}
