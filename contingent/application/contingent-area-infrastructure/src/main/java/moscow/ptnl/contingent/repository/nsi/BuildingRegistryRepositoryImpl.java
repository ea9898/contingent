package moscow.ptnl.contingent.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.NsiBuildingRegistry;
import moscow.ptnl.contingent.area.entity.nsi.NsiBuildingRegistry_;
import moscow.ptnl.contingent.repository.BaseRepository;
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
