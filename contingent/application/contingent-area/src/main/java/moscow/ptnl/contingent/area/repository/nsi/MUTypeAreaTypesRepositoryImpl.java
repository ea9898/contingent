package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.AreaType_;
import moscow.ptnl.contingent.area.entity.nsi.MUTypeAreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.MUTypeAreaTypes_;
import moscow.ptnl.contingent.area.model.nsi.AvailableToCreateType;
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
    public List<MUTypeAreaTypes> findMuTypeAreaTypes(List<Long> muTypes, List<Long> areaTypeCodes, AvailableToCreateType availableToCreate) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<MUTypeAreaTypes> criteria = criteriaBuilder.createQuery(MUTypeAreaTypes.class);
        Root<MUTypeAreaTypes> template = criteria.from(MUTypeAreaTypes.class);
        criteria.where(
                criteriaBuilder.and(
                        muTypes == null || muTypes.isEmpty() ? criteriaBuilder.conjunction() :
                                template.get(MUTypeAreaTypes_.muTypeId.getName()).in(muTypes),
                        areaTypeCodes == null || areaTypeCodes.isEmpty() ? criteriaBuilder.conjunction() :
                                template.get(MUTypeAreaTypes_.areaType.getName()).get(AreaType_.code.getName()).in(areaTypeCodes),
                        availableToCreate == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(template.get(MUTypeAreaTypes_.availableToCreate.getName()), availableToCreate.getValue())
                )
        );

        return entityManager.createQuery(criteria).getResultList();
    }

    @Override
    public List<MUTypeAreaTypes> findMuTypeAreaTypes(Long muTypeId, List<Long> areaTypeCodes, AvailableToCreateType availableToCreate) {
        return findMuTypeAreaTypes(Collections.singletonList(muTypeId), areaTypeCodes, availableToCreate);
    }

}
