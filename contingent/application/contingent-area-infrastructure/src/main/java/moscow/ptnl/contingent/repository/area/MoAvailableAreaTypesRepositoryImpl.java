package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes_;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
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

    @Autowired
    private MoAvailableAreaTypesCRUDRepository moAvailableAreaTypesCRUDRepository;

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

    @Override
    public List<MoAvailableAreaTypes> findByAreaTypes(AreaType areaType, Long moId) {
        Specification<MoAvailableAreaTypes> specification =
                (root, criteriaQuery, criteriaBuilder) ->
                        criteriaBuilder.and(
                                criteriaBuilder.equal(root.get(MoAvailableAreaTypes_.areaType.getName()), areaType),
                                moId == null ? criteriaBuilder.conjunction() : criteriaBuilder.equal(root.get(MoAvailableAreaTypes_.moId.getName()), moId)
                        );

        return moAvailableAreaTypesCRUDRepository.findAll(specification);
    }

}
