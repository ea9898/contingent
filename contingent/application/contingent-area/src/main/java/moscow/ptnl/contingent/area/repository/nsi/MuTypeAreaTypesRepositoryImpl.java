package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.AreaType_;
import moscow.ptnl.contingent.area.entity.nsi.MuTypeAreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.MuTypeAreaTypes_;
import moscow.ptnl.contingent.area.model.nsi.AvailableToCreateType;
import moscow.ptnl.contingent.repository.BaseRepository;
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
    public List<MuTypeAreaTypes> findMuTypeAreaTypes(List<Long> muTypes, List<Long> areaTypeCodes, AvailableToCreateType availableToCreate) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<MuTypeAreaTypes> criteria = criteriaBuilder.createQuery(MuTypeAreaTypes.class);
        Root<MuTypeAreaTypes> template = criteria.from(MuTypeAreaTypes.class);
        criteria.where(
                criteriaBuilder.and(
                        muTypes == null || muTypes.isEmpty() ? criteriaBuilder.conjunction() :
                                template.get(MuTypeAreaTypes_.muTypeId.getName()).in(muTypes),
                        areaTypeCodes == null || areaTypeCodes.isEmpty() ? criteriaBuilder.conjunction() :
                                template.get(MuTypeAreaTypes_.areaType.getName()).get(AreaType_.code.getName()).in(areaTypeCodes),
                        availableToCreate == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(template.get(MuTypeAreaTypes_.availableToCreate.getName()), availableToCreate.getValue())
                )
        );

        return entityManager.createQuery(criteria).getResultList();
    }

    @Override
    public List<MuTypeAreaTypes> findMuTypeAreaTypes(Long muTypeId, List<Long> areaTypeCodes, AvailableToCreateType availableToCreate) {
        return findMuTypeAreaTypes(Collections.singletonList(muTypeId), areaTypeCodes, availableToCreate);
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
