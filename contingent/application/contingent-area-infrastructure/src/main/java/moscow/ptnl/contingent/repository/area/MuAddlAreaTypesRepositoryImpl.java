package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.MuAddlAreaTypes;

import moscow.ptnl.contingent.domain.area.entity.MuAddlAreaTypes_;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import java.util.List;
import moscow.ptnl.contingent.nsi.domain.area.AreaType_;

@Repository
@Transactional(propagation = Propagation.MANDATORY)
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
                    muIds == null || muIds.isEmpty() 
                            ? criteriaBuilder.conjunction() 
                            : criteriaBuilder.in(profile.get(MuAddlAreaTypes_.muId.getName())).value(muIds), //profile.get(MuAddlAreaTypes_.muId.getName()).in(muIds),
                    criteriaBuilder.in(profile.get(MuAddlAreaTypes_.areaType.getName()).get(AreaType_.code.getName())).value(areaTypes) //profile.get(MuAddlAreaTypes_.areaType.getName()).in(areaTypes)
            )
        );

        return entityManager.createQuery(criteria).getResultList();
    }
}
