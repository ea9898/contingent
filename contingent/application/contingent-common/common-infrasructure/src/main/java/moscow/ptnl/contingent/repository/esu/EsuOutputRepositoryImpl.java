package moscow.ptnl.contingent.repository.esu;

import moscow.ptnl.contingent.domain.esu.EsuStatusType;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;
import jakarta.persistence.criteria.CriteriaUpdate;
import moscow.ptnl.contingent.domain.esu.EsuOutput;
import moscow.ptnl.contingent.domain.esu.EsuOutput_;
import moscow.ptnl.contingent.repository.CommonSpecification;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class EsuOutputRepositoryImpl extends BaseRepository implements EsuOutputRepository {
    
    @Override
    public List<EsuOutput> findEsuOutputsToResend(LocalDateTime olderThan) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        
        CriteriaQuery<EsuOutput> selectCriteria = criteriaBuilder.createQuery(EsuOutput.class);
        Root<EsuOutput> template = selectCriteria.from(EsuOutput.class);
        
        selectCriteria.where(
            criteriaBuilder.and(
                criteriaBuilder.equal(template.get(EsuOutput_.status.getName()), EsuStatusType.UNSUCCESS),
                criteriaBuilder.or(
                    criteriaBuilder.lessThan(template.get(EsuOutput_.sentTime.getName()), olderThan),
                    criteriaBuilder.isNull(template.get(EsuOutput_.sentTime.getName()))
                )
            )
        );
        List<EsuOutput> results = entityManager
                .createQuery(selectCriteria)
                .setMaxResults(CommonSpecification.MAX_RECORDS_IN_CLAUSE)
                .getResultList();
        
        if (!results.isEmpty()) {
            CriteriaUpdate<EsuOutput> updateCriteria = criteriaBuilder.createCriteriaUpdate(EsuOutput.class);
            template = updateCriteria.from(EsuOutput.class);

            List<Long> resultsIds = results.stream().map(r -> r.getId()).collect(Collectors.toList());
            updateCriteria
                    .set(template.get(EsuOutput_.status.getName()), EsuStatusType.INPROGRESS)
                    .where(criteriaBuilder.in(template.get(EsuOutput_.id.getName())).value(resultsIds)); //template.get(EsuOutput_.id).in(resultsIds)

            entityManager
                .createQuery(updateCriteria)
                .executeUpdate();
        }
        
        return results;
    }

    @Override
    public void updateStatus(List<Long> ids, EsuStatusType fromStatus, EsuStatusType toStatus, String hostName) {
        if (ids == null || ids.isEmpty())
            return;
        
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        
        CriteriaUpdate<EsuOutput> updateCriteria = criteriaBuilder.createCriteriaUpdate(EsuOutput.class);
        Root<EsuOutput> template = updateCriteria.from(EsuOutput.class);
        
        updateCriteria
                .set(template.get(EsuOutput_.status.getName()), toStatus)
                .set(template.get(EsuOutput_.host.getName()), hostName)
                .where(
                    criteriaBuilder.and(
                        criteriaBuilder.equal(template.get(EsuOutput_.status.getName()), fromStatus),
                        criteriaBuilder.in(template.get(EsuOutput_.id.getName())).value(ids) //template.get(EsuOutput_.id).in(ids)
                    )
                );
        
        entityManager
                .createQuery(updateCriteria)
                .executeUpdate();
    }

    @Override
    public void updateStatus(Long id, EsuStatusType fromStatus, EsuStatusType toStatus, String hostName) {
        if (id == null)
            return;
        
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        
        CriteriaUpdate<EsuOutput> updateCriteria = criteriaBuilder.createCriteriaUpdate(EsuOutput.class);
        Root<EsuOutput> template = updateCriteria.from(EsuOutput.class);
        
        updateCriteria
                .set(template.get(EsuOutput_.status.getName()), toStatus)
                .set(template.get(EsuOutput_.host.getName()), hostName)
                .where(
                    criteriaBuilder.and(
                        criteriaBuilder.equal(template.get(EsuOutput_.status.getName()), fromStatus),
                        criteriaBuilder.equal(template.get(EsuOutput_.id), id)
                    )
                );
        
        entityManager
                .createQuery(updateCriteria)
                .executeUpdate();
    }
    
    @Override
    public void updateMessage(Long id, String message, String method) {
        if (id == null)
            throw new IllegalArgumentException("идентификатор записи не может быть null");
        
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        
        CriteriaUpdate<EsuOutput> updateCriteria = criteriaBuilder.createCriteriaUpdate(EsuOutput.class);
        Root<EsuOutput> template = updateCriteria.from(EsuOutput.class);
        
        updateCriteria
                .set(template.get(EsuOutput_.message), message)
                .set(template.get(EsuOutput_.method), method)
                .where(criteriaBuilder.equal(template.get(EsuOutput_.id), id));
        
        entityManager
            .createQuery(updateCriteria)
            .executeUpdate();
    }
}
