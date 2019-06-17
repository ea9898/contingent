package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.MuAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.area.MuAvailableAreaTypes_;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
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
public class MuAvailableAreaTypesRepositoryImpl extends BaseRepository implements MuAvailableAreaTypesRepository {

    @Autowired
    private MuAvailableAreaTypesCRUDRepository muAvailableAreaTypesCRUDRepository;

    @Override
    public List<MuAvailableAreaTypes> findAreaTypes(long muId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<MuAvailableAreaTypes> criteria = criteriaBuilder.createQuery(MuAvailableAreaTypes.class);
        Root<MuAvailableAreaTypes> order = criteria.from(MuAvailableAreaTypes.class);
        criteria.where(
                criteriaBuilder.equal(order.get(MuAvailableAreaTypes_.muId.getName()), muId)
        );
        return entityManager.createQuery(criteria).getResultList();
    }

    @Override
    public List<MuAvailableAreaTypes> findByAreaTypes(AreaType areaType) {
        Specification<MuAvailableAreaTypes> specification =
                (root, criteriaQuery, criteriaBuilder) ->
                        criteriaBuilder.equal(root.get(MuAvailableAreaTypes_.areaType.getName()), areaType);

        return muAvailableAreaTypesCRUDRepository.findAll(specification);
    }
}
