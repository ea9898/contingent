package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.nsi.domain.entity.NsiPushEvent;
import moscow.ptnl.contingent.repository.CommonRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface NsiPushEventCRUDRepository extends CommonRepository<NsiPushEvent, Long> {

}
