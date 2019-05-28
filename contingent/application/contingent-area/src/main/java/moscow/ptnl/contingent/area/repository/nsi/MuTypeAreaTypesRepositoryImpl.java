package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.AreaType_;
import moscow.ptnl.contingent.area.entity.nsi.MuTypeAreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.MuTypeAreaTypes_;
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
public class MuTypeAreaTypesRepositoryImpl extends BaseRepository implements MuTypeAreaTypesRepository {

    @Override
    public MuTypeAreaTypes findMuProfileTemplate(int muTypeId, Long areaTypeCode) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<MuTypeAreaTypes> criteria = criteriaBuilder.createQuery(MuTypeAreaTypes.class);
        Root<MuTypeAreaTypes> template = criteria.from(MuTypeAreaTypes.class);
        criteria.where(
                criteriaBuilder.and(
                        criteriaBuilder.equal(template.get(MuTypeAreaTypes_.muTypeId.getName()), muTypeId),
                        criteriaBuilder.equal(template.get(MuTypeAreaTypes_.areaType.getName()).get(AreaType_.code.getName()), areaTypeCode)
                )
        );
        List<MuTypeAreaTypes> results = entityManager.createQuery(criteria).getResultList();

        return results.isEmpty() ? null : results.get(0);
    }

    @Override
    public List<MuTypeAreaTypes> findMuProfileTemplates(Long muTypeId, List<Long> areaTypeCodes, Boolean availableToCreate) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<MuTypeAreaTypes> criteria = criteriaBuilder.createQuery(MuTypeAreaTypes.class);
        Root<MuTypeAreaTypes> template = criteria.from(MuTypeAreaTypes.class);
        criteria.where(
                criteriaBuilder.and(
                        criteriaBuilder.equal(template.get(MuTypeAreaTypes_.muTypeId.getName()), muTypeId),
                        areaTypeCodes.isEmpty() ? criteriaBuilder.conjunction() :
                                template.get(MuTypeAreaTypes_.areaType.getName()).get(AreaType_.code.getName()).in(areaTypeCodes),
                        availableToCreate == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(template.get(MuTypeAreaTypes_.availableToCreate.getName()), availableToCreate)
                )
        );

        return entityManager.createQuery(criteria).getResultList();
    }

    @Override
    public List<MuTypeAreaTypes> findMuTypeAreaTypes(Long muTypeId, List<Long> areaTypeCodes) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<MuTypeAreaTypes> criteria = criteriaBuilder.createQuery(MuTypeAreaTypes.class);
        Root<MuTypeAreaTypes> template = criteria.from(MuTypeAreaTypes.class);
        criteria.where(
                criteriaBuilder.and(
                        criteriaBuilder.equal(template.get(MuTypeAreaTypes_.muTypeId), muTypeId),
                        areaTypeCodes.isEmpty() ? criteriaBuilder.conjunction() :
                                template.get(MuTypeAreaTypes_.areaType).get(AreaType_.code).in(areaTypeCodes)
                )
        );

        return entityManager.createQuery(criteria).getResultList();
    }

}
