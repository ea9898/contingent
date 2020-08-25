package moscow.ptnl.contingent.repository.trigger;

import moscow.ptnl.contingent.domain.trigger.TriggerStatus;
import moscow.ptnl.contingent.repository.CommonRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface TriggerStatusCRUDRepository extends CommonRepository<TriggerStatus, String> {
    
}
