package moscow.ptnl.contingent.repository.sysop;

import moscow.ptnl.contingent.area.entity.sysop.Sysop;
import moscow.ptnl.contingent.area.entity.sysop.SysopMsg;
import moscow.ptnl.contingent.repository.PagingAndSortingRepository;
import org.springframework.data.repository.NoRepositoryBean;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;
import java.util.Set;

@NoRepositoryBean
public interface SysopMsgRepository {


    List<SysopMsg> getSysopMsgBySysop(Sysop sysop);

    Map<SysopMsg, List<SysopMsg>> getSysopMsgChildsMap(List<SysopMsg> sysopMsgs);
}
