package moscow.ptnl.contingent.repository.trigger;

import moscow.ptnl.contingent.domain.trigger.TriggerHistoryItem;
import moscow.ptnl.contingent.repository.CommonRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface TriggerHistoryItemCRUDRepository extends CommonRepository<TriggerHistoryItem, Long> {
    
}
