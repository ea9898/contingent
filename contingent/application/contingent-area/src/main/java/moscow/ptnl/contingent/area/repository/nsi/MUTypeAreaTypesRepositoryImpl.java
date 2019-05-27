package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.AreaType_;
import moscow.ptnl.contingent.area.entity.nsi.MUTypeAreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.MUTypeAreaTypes_;
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
public class MUTypeAreaTypesRepositoryImpl extends BaseRepository implements MUTypeAreaTypesRepository {

    @Override
    public MUTypeAreaTypes findMuProfileTemplate(int muTypeId, Long areaTypeCode) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<MUTypeAreaTypes> criteria = criteriaBuilder.createQuery(MUTypeAreaTypes.class);
        Root<MUTypeAreaTypes> template = criteria.from(MUTypeAreaTypes.class);
        criteria.where(
                criteriaBuilder.and(
                        criteriaBuilder.equal(template.get(MUTypeAreaTypes_.muTypeId.getName()), muTypeId),
                        criteriaBuilder.equal(template.get(MUTypeAreaTypes_.areaType.getName()).get(AreaType_.code.getName()), areaTypeCode)
                )
        );
        List<MUTypeAreaTypes> results = entityManager.createQuery(criteria).getResultList();

        return results.isEmpty() ? null : results.get(0);
    }

    @Override
    public List<MUTypeAreaTypes> findMuProfileTemplates(Long muTypeId, List<Long> areaTypeCodes, Boolean availableToCreate) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<MUTypeAreaTypes> criteria = criteriaBuilder.createQuery(MUTypeAreaTypes.class);
        Root<MUTypeAreaTypes> template = criteria.from(MUTypeAreaTypes.class);
        criteria.where(
                criteriaBuilder.and(
                        criteriaBuilder.equal(template.get(MUTypeAreaTypes_.muTypeId.getName()), muTypeId),
                        areaTypeCodes.isEmpty() ? criteriaBuilder.conjunction() :
                                template.get(MUTypeAreaTypes_.areaType.getName()).get(AreaType_.code.getName()).in(areaTypeCodes),
                        availableToCreate == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(template.get(MUTypeAreaTypes_.availableToCreate.getName()), availableToCreate)
                )
        );

        return entityManager.createQuery(criteria).getResultList();
    }



}
