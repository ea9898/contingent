package moscow.ptnl.contingent.repository.history;

import moscow.ptnl.contingent.domain.history.HistoryRequest;
import moscow.ptnl.contingent.repository.CommonRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 * @author sorlov
 */
@Repository
@Transactional(propagation = Propagation.MANDATORY)
public interface HistoryRequestsRepository extends CommonRepository<HistoryRequest, Long> {
    
}
