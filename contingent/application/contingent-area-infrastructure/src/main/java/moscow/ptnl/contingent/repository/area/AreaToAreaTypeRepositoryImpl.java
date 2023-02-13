package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaToAreaType;
import moscow.ptnl.contingent.domain.area.entity.AreaToAreaType_;
import moscow.ptnl.contingent.domain.area.entity.Area_;
import moscow.ptnl.contingent.domain.area.repository.AreaToAreaTypeRepository;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import java.util.List;
import moscow.ptnl.contingent.nsi.domain.area.AreaType_;

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
    public List<AreaToAreaType> findAreaTypesByAreaAndTypeCode(Area area, List<Long> areaTypes) {
        Specification<AreaToAreaType> specification = (root, criteriaQuery, criteriaBuilder) ->
            criteriaBuilder.and(
                criteriaBuilder.equal(root.get(AreaToAreaType_.area), area),
                criteriaBuilder.in(root.get(AreaToAreaType_.areaType.getName()).get(AreaType_.code.getName())).value(areaTypes) //root.get(AreaToAreaType_.areaType).in(areaTypes)
            );
        return areaToAreaTypeCRUDRepository.findAll(specification);
    }

    @Override
    public AreaToAreaType save(AreaToAreaType areaToAreaType) {
        return areaToAreaTypeCRUDRepository.save(areaToAreaType);
    }

    @Override
    public void deleteAll(List<AreaToAreaType> areaToAreaTypes) {
        areaToAreaTypeCRUDRepository.deleteAll(areaToAreaTypes);
    }
}
