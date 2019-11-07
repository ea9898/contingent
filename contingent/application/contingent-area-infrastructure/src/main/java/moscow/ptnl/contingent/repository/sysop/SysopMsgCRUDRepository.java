package moscow.ptnl.contingent.repository.sysop;

import moscow.ptnl.contingent.area.entity.sysop.SysopMsg;
import moscow.ptnl.contingent.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface SysopMsgCRUDRepository extends PagingAndSortingRepository<SysopMsg, Long> {
}