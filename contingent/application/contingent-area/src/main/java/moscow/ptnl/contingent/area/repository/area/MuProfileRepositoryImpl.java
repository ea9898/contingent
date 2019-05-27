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
public class MuProfileRepositoryImpl extends BaseRepository implements MuProfileRepository {

    @Override
    public List<MuAddlAreaTypes> getMuProfilesByMuId(long muId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<MuAddlAreaTypes> criteria = criteriaBuilder.createQuery(MuAddlAreaTypes.class);
        Root<MuAddlAreaTypes> profile = criteria.from(MuAddlAreaTypes.class);
        criteria.where(criteriaBuilder.equal(profile.get(MuAddlAreaTypes_.muId.getName()), muId));

        return entityManager.createQuery(criteria).getResultList();
    }

    @Override
    public List<MuAddlAreaTypes> findMuProfilesByMuIdAndAreaTypes(long muId, List<Long> areaTypes) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<MuAddlAreaTypes> criteria = criteriaBuilder.createQuery(MuAddlAreaTypes.class);
        Root<MuAddlAreaTypes> profile = criteria.from(MuAddlAreaTypes.class);
        criteria.where(
            criteriaBuilder.and(
                    criteriaBuilder.equal(profile.get(MuAddlAreaTypes_.muId.getName()), muId),
                    profile.get(MuAddlAreaTypes_.areaType.getName()).in(areaTypes)
            )
        );

        return entityManager.createQuery(criteria).getResultList();
    }

}
