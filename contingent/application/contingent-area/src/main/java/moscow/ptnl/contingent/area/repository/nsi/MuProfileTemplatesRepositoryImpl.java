package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.AreaTypes_;
import moscow.ptnl.contingent.area.entity.nsi.MUProfileTemplates;
import moscow.ptnl.contingent.area.entity.nsi.MUProfileTemplates_;
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
public class MuProfileTemplatesRepositoryImpl extends BaseRepository implements MuProfileTemplatesRepository {

    @Override
    public MUProfileTemplates findMuProfileTemplate(int muTypeId, Long areaTypeCode) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<MUProfileTemplates> criteria = criteriaBuilder.createQuery(MUProfileTemplates.class);
        Root<MUProfileTemplates> template = criteria.from(MUProfileTemplates.class);
        criteria.where(
                criteriaBuilder.and(
                        criteriaBuilder.equal(template.get(MUProfileTemplates_.muTypeId.getName()), muTypeId),
                        criteriaBuilder.equal(template.get(MUProfileTemplates_.areaType.getName()).get(AreaTypes_.code.getName()), areaTypeCode)
                )
        );
        List<MUProfileTemplates> results = entityManager.createQuery(criteria).getResultList();

        return results.isEmpty() ? null : results.get(0);
    }
}
