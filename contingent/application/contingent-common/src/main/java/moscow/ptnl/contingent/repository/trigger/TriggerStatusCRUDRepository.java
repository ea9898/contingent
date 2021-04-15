package moscow.ptnl.contingent.repository.trigger;

import moscow.ptnl.contingent.domain.trigger.TriggerName;
import moscow.ptnl.contingent.domain.trigger.TriggerStatus;
import moscow.ptnl.contingent.repository.CommonRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.LockModeType;
import java.util.Optional;
import javax.persistence.QueryHint;
import org.springframework.data.jpa.repository.QueryHints;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface TriggerStatusCRUDRepository extends CommonRepository<TriggerStatus, TriggerName> {

    @Query("SELECT s FROM TriggerStatus s WHERE s.trigger = :triggerName")
    @QueryHints(value = {
        @QueryHint(name = "javax.persistence.lock.timeout", value = "0") //время в мсек ожидания снятия лока с заблокированной записи (0 аналог NOWAIT, потом Exception) 
    })
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Optional<TriggerStatus> findWithLock(@Param("triggerName") TriggerName triggerName);
}
