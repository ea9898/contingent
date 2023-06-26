package moscow.ptnl.contingent.repository.history;

import moscow.ptnl.contingent.nsi.domain.area.EventTypes;
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
public interface EventTypesCRUDRepository extends CommonRepository<EventTypes, Long> {

}