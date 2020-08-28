package moscow.ptnl.contingent.repository.esu;

import java.time.LocalDateTime;
import moscow.ptnl.contingent.domain.esu.EsuInput;
import moscow.ptnl.contingent.domain.esu.EsuStatusType;
import moscow.ptnl.contingent.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface EsuInputCRUDRepository extends PagingAndSortingRepository<EsuInput, Long> {
    
    long deleteByReceivedTimeBeforeAndStatus(LocalDateTime receivedTime, EsuStatusType status);
    
    long deleteByReceivedTimeBefore(LocalDateTime receivedTime);
    
}
