package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.Area_;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypes_;
import moscow.ptnl.contingent.area.repository.BaseRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.util.Collections;
import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class AreaRepositoryImpl extends BaseRepository implements AreaRepository {

    @Override
    public List<Area> findAreas(Long moId, Long muId, List<Long> areaTypeCodes, Integer number, Boolean actual) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Area> criteria = criteriaBuilder.createQuery(Area.class);
        Root<Area> profile = criteria.from(Area.class);
        criteria.where(
                criteriaBuilder.and(
                        moId == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(profile.get(Area_.moId.getName()), moId),
                        muId == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(profile.get(Area_.muId.getName()), muId),
                        number == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(profile.get(Area_.number.getName()), number),
                        areaTypeCodes == null || areaTypeCodes.isEmpty() ? criteriaBuilder.conjunction() :
                                profile.get(Area_.areaType.getName()).get(AreaTypes_.code.getName()).in(areaTypeCodes),
                        actual == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(profile.get(Area_.actual.getName()), actual)
                )
        );
        return entityManager.createQuery(criteria).getResultList();
    }

    @Override
    public List<Area> findAreas(Long moId, Long muId, Long areaTypeCode, Integer number, Boolean actual) {
        return findAreas(moId, muId, Collections.singletonList(areaTypeCode), number, actual);
    }
}
