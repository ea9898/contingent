package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.MuAddlAreaTypes;
import moscow.ptnl.contingent.area.entity.area.MuAddlAreaTypes_;
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
public class MuAddlAreaTypesRepositoryImpl extends BaseRepository implements MuAddlAreaTypesRepository {

    @Override
    public List<MuAddlAreaTypes> getMuAddlAreaTypes(long muId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<MuAddlAreaTypes> criteria = criteriaBuilder.createQuery(MuAddlAreaTypes.class);
        Root<MuAddlAreaTypes> profile = criteria.from(MuAddlAreaTypes.class);
        criteria.where(criteriaBuilder.equal(profile.get(MuAddlAreaTypes_.muId.getName()), muId));

        return entityManager.createQuery(criteria).getResultList();
    }

    @Override
    public List<MuAddlAreaTypes> findMuAddlAreaTypes(List<Long> muIds, List<Long> areaTypes) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<MuAddlAreaTypes> criteria = criteriaBuilder.createQuery(MuAddlAreaTypes.class);
        Root<MuAddlAreaTypes> profile = criteria.from(MuAddlAreaTypes.class);
        criteria.where(
            criteriaBuilder.and(
                    muIds == null || muIds.isEmpty() ? criteriaBuilder.conjunction() :
                            profile.get(MuAddlAreaTypes_.muId.getName()).in(muIds),
                    profile.get(MuAddlAreaTypes_.areaType.getName()).in(areaTypes)
            )
        );

        return entityManager.createQuery(criteria).getResultList();
    }

}
