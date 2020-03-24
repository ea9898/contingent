package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.MuAvailableAreaTypes;
import moscow.ptnl.contingent.domain.area.entity.MuAvailableAreaTypes_;
import moscow.ptnl.contingent.domain.area.repository.MuAvailableAreaTypesRepository;
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
    public List<MuAvailableAreaTypes> findByAreaTypes(AreaType areaType, Long muId) {
        Specification<MuAvailableAreaTypes> specification =
                (root, criteriaQuery, criteriaBuilder) ->
                        criteriaBuilder.and(
                                criteriaBuilder.equal(root.get(MuAvailableAreaTypes_.areaType.getName()), areaType),
                                muId == null ? criteriaBuilder.conjunction() : criteriaBuilder.equal(root.get(MuAvailableAreaTypes_.muId.getName()), muId)
                        );

        return muAvailableAreaTypesCRUDRepository.findAll(specification);
    }

    @Override
    public MuAvailableAreaTypes save(MuAvailableAreaTypes muAvailableAreaTypes) {
        return muAvailableAreaTypesCRUDRepository.save(muAvailableAreaTypes);
    }

    @Override
    public void deleteAll(List<MuAvailableAreaTypes> muAvailableAreaTypes) {
        muAvailableAreaTypesCRUDRepository.deleteAll(muAvailableAreaTypes);
    }

}
