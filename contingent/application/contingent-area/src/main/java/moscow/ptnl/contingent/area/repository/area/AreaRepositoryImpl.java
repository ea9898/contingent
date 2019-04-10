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
import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class AreaRepositoryImpl extends BaseRepository implements AreaRepository {

    @Override
    public List<Area> findAreas(long muId, String areaTypeCode, Boolean actual) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Area> criteria = criteriaBuilder.createQuery(Area.class);
        Root<Area> profile = criteria.from(Area.class);
        criteria.where(
                criteriaBuilder.and(
                        criteriaBuilder.equal(profile.get(Area_.moId.getName()), muId),
                        areaTypeCode == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(profile.get(Area_.areaType.getName()).get(AreaTypes_.code.getName()), areaTypeCode),
                        actual == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(profile.get(Area_.actual.getName()), actual)
                )
        );
        return entityManager.createQuery(criteria).getResultList();
    }
}
