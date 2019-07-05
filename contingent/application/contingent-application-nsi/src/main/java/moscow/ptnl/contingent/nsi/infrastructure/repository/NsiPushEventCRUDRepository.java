package moscow.ptnl.contingent.nsi.infrastructure.repository;

import moscow.ptnl.contingent.nsi.infrastructure.CommonRepository;
import moscow.ptnl.contingent.nsi.pushaccepter.NsiPushEvent;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface NsiPushEventCRUDRepository extends CommonRepository<NsiPushEvent, Long> {

}
