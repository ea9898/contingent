package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes_;
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
public class MoAvailableAreaTypesRepositoryImpl extends BaseRepository implements MoAvailableAreaTypesRepository {

    @Override
    public List<MoAvailableAreaTypes> findAreaTypes(long moId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<MoAvailableAreaTypes> criteria = criteriaBuilder.createQuery(MoAvailableAreaTypes.class);
        Root<MoAvailableAreaTypes> order = criteria.from(MoAvailableAreaTypes.class);
        criteria.where(
                criteriaBuilder.equal(order.get(MoAvailableAreaTypes_.moId.getName()), moId)
        );
        return entityManager.createQuery(criteria).getResultList();
    }
}
