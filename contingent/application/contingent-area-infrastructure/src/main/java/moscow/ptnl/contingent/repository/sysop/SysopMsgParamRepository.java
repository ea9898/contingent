package moscow.ptnl.contingent.repository.sysop;

import moscow.ptnl.contingent.area.entity.sysop.Sysop;
import moscow.ptnl.contingent.area.entity.sysop.SysopMsg;
import moscow.ptnl.contingent.area.entity.sysop.SysopMsgParam;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@NoRepositoryBean
public interface SysopMsgParamRepository {

    Map<SysopMsg, List<SysopMsgParam>> getSysopMsgParamsBySysopMsgList(List<SysopMsg> sysop);

}
