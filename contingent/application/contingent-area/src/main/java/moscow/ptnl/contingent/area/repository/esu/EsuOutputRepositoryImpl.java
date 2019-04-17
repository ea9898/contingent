package moscow.ptnl.contingent.area.repository.esu;

import moscow.ptnl.contingent.area.entity.esu.EsuOutput;
import moscow.ptnl.contingent.area.entity.esu.EsuOutput_;
import moscow.ptnl.contingent.area.repository.BaseRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.util.Date;
import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class EsuOutputRepositoryImpl extends BaseRepository implements EsuOutputRepository {

    @Override
    public List<EsuOutput> findEsuOutputsToResend(Date olderThan) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<EsuOutput> criteria = criteriaBuilder.createQuery(EsuOutput.class);
        Root<EsuOutput> template = criteria.from(EsuOutput.class);

        criteria.where(
                criteriaBuilder.and(
                        criteriaBuilder.equal(template.get(EsuOutput_.status.getName()), 0),
                        criteriaBuilder.or(
                                criteriaBuilder.lessThan(template.get(EsuOutput_.sentTime.getName()), olderThan),
                                criteriaBuilder.isNull(template.get(EsuOutput_.sentTime.getName()))
                        )
                )
        );
        List<EsuOutput> results = entityManager.createQuery(criteria).getResultList();
        results.forEach(r -> r.setStatus(2));

        return results;
    }
}
