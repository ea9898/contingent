package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.RegistryBuilding;
import moscow.ptnl.contingent.area.entity.nsi.RegistryBuilding_;
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
public class RegistryBuildingRepositoryImpl extends BaseRepository implements RegistryBuildingRepository {

    @Override
    public List<RegistryBuilding> getRegistryBuildings(long globalId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<RegistryBuilding> criteria = criteriaBuilder.createQuery(RegistryBuilding.class);
        Root<RegistryBuilding> template = criteria.from(RegistryBuilding.class);
        criteria.where(
                criteriaBuilder.and(
                        criteriaBuilder.equal(template.get(RegistryBuilding_.globalId.getName()), globalId)
                )
        );
        return entityManager.createQuery(criteria).getResultList();
    }

    @Override
    public List<RegistryBuilding> findRegistryBuildings(String l1Value, String l2Value, String l3Value, long addrId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<RegistryBuilding> criteria = criteriaBuilder.createQuery(RegistryBuilding.class);
        Root<RegistryBuilding> template = criteria.from(RegistryBuilding.class);
        criteria.where(
                criteriaBuilder.and(
                        criteriaBuilder.equal(template.get(RegistryBuilding_.l1Value.getName()), l1Value),
                        criteriaBuilder.equal(template.get(RegistryBuilding_.l2Value.getName()), l2Value),
                        criteriaBuilder.equal(template.get(RegistryBuilding_.l3Value.getName()), l3Value),
                        criteriaBuilder.equal(template.get(RegistryBuilding_.addrId.getName()), addrId)
                )
        );
        return entityManager.createQuery(criteria).getResultList();
    }
}
