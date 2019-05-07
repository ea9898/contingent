package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.MuProfile;
import moscow.ptnl.contingent.area.entity.area.MuProfile_;
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
    public List<MuProfile> getMuProfilesByMuId(long muId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<MuProfile> criteria = criteriaBuilder.createQuery(MuProfile.class);
        Root<MuProfile> profile = criteria.from(MuProfile.class);
        criteria.where(criteriaBuilder.equal(profile.get(MuProfile_.muId.getName()), muId));

        return entityManager.createQuery(criteria).getResultList();
    }

    @Override
    public List<MuProfile> findMuProfilesByMuIdAndAreaTypes(long muId, List<Long> areaTypes) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<MuProfile> criteria = criteriaBuilder.createQuery(MuProfile.class);
        Root<MuProfile> profile = criteria.from(MuProfile.class);
        criteria.where(
            criteriaBuilder.and(
                    criteriaBuilder.equal(profile.get(MuProfile_.muId.getName()), muId),
                    profile.get(MuProfile_.areaType.getName()).in(areaTypes)
            )
        );

        return entityManager.createQuery(criteria).getResultList();
    }

}
