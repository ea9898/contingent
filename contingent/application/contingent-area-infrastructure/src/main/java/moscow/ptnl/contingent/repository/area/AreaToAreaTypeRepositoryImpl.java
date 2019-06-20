package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType_;
import moscow.ptnl.contingent.area.entity.area.Area_;
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
public class AreaToAreaTypeRepositoryImpl extends BaseRepository implements AreaToAreaTypeRepository {

    @Autowired
    AreaToAreaTypeCRUDRepository areaToAreaTypeCRUDRepository;

    @Override
    public List<AreaToAreaType> getAreaTypesByAreaId(long areaId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<AreaToAreaType> criteria = criteriaBuilder.createQuery(AreaToAreaType.class);
        Root<AreaToAreaType> profile = criteria.from(AreaToAreaType.class);
        criteria.where(criteriaBuilder.equal(profile.get(AreaToAreaType_.area.getName()).get(Area_.id.getName()), areaId));

        return entityManager.createQuery(criteria).getResultList();
    }

    @Override
    public List<AreaToAreaType> findAreaTypesByAreaAndTypeCode(Area area, List<AreaType> areaType) {
        Specification<AreaToAreaType> specification = (root, criteriaQuery, criteriaBuilder) ->
            criteriaBuilder.and(
                criteriaBuilder.equal(root.get(AreaToAreaType_.area), area),
                root.get(AreaToAreaType_.areaType).in(areaType)
            );

        return areaToAreaTypeCRUDRepository.findAll(specification);
    }
}
