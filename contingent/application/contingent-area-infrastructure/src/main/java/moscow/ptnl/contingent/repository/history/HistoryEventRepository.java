package moscow.ptnl.contingent.repository.history;

import moscow.ptnl.contingent.domain.history.HistoryEvent;
import moscow.ptnl.contingent.repository.CommonRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 * @author m.kachalov
 */
@Repository
@Transactional(propagation = Propagation.MANDATORY)
public interface HistoryEventRepository extends CommonRepository<HistoryEvent, Long> {
    
}
