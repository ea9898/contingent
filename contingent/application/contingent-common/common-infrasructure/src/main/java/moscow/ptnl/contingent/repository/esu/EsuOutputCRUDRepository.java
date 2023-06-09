package moscow.ptnl.contingent.repository.esu;

import java.time.LocalDateTime;
import moscow.ptnl.contingent.domain.esu.EsuOutput;
import moscow.ptnl.contingent.domain.esu.EsuStatusType;
import moscow.ptnl.contingent.repository.CommonRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface EsuOutputCRUDRepository extends CommonRepository<EsuOutput, Long> {
    
    long deleteBySentTimeBeforeAndStatus(LocalDateTime sentTime, EsuStatusType status);
    
    long deleteBySentTimeBefore(LocalDateTime sentTime);
    
}
