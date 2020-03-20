package moscow.ptnl.contingent.repository;

import moscow.ptnl.contingent.sysop.entity.Sysop;
import moscow.ptnl.contingent.sysop.entity.SysopMsg;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;
import java.util.Map;

@NoRepositoryBean
public interface SysopMsgRepository {


    List<SysopMsg> getSysopMsgBySysop(Sysop sysop);

    Map<SysopMsg, List<SysopMsg>> getSysopMsgChildsMap(List<SysopMsg> sysopMsgs);
}
