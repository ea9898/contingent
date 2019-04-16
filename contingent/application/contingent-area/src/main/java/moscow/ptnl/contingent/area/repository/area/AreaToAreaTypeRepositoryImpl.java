package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType_;
import moscow.ptnl.contingent.area.entity.area.Area_;
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
public class AreaToAreaTypeRepositoryImpl extends BaseRepository implements AreaToAreaTypeRepository {

    @Override
    public List<AreaToAreaType> getAreaTypesByAreaId(long areaId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<AreaToAreaType> criteria = criteriaBuilder.createQuery(AreaToAreaType.class);
        Root<AreaToAreaType> profile = criteria.from(AreaToAreaType.class);
        criteria.where(criteriaBuilder.equal(profile.get(AreaToAreaType_.area.getName()).get(Area_.id.getName()), areaId));

        return entityManager.createQuery(criteria).getResultList();
    }
}
