package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeMedicalPositions_;
import moscow.ptnl.contingent.area.repository.BaseRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.util.List;

@Repository
@Transactional(propagation= Propagation.MANDATORY)
public class AreaTypeMedicalPositionsRepositoryImpl extends BaseRepository implements AreaTypeMedicalPositionsRepository {
    @Override
    public List<AreaTypeMedicalPositions> getPositionsByAreaType(long areaTypeId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<AreaTypeMedicalPositions> criteria = criteriaBuilder.createQuery(AreaTypeMedicalPositions.class);
        Root<AreaTypeMedicalPositions> template = criteria.from(AreaTypeMedicalPositions.class);
        criteria.where(
                criteriaBuilder.equal(template.get(AreaTypeMedicalPositions_.areaType.getName()), areaTypeId)
        );
        List<AreaTypeMedicalPositions> results = entityManager.createQuery(criteria).getResultList();

        return results.isEmpty() ? null : results;
    }
}
