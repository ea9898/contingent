package moscow.ptnl.contingent.nsi.pushaccepter;

import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface NsiPushEventCRUDRepository extends CrudRepository<NsiPushEvent, Long> {

}
