package moscow.ptnl.contingent.area.repository.esu;

import moscow.ptnl.contingent.area.entity.esu.EsuOutput;
import moscow.ptnl.contingent.area.repository.CommonRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface EsuOutputCRUDRepository extends CommonRepository<EsuOutput, Long> {
}
