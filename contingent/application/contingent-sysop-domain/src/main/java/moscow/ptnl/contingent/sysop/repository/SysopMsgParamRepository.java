package moscow.ptnl.contingent.sysop.repository;

import moscow.ptnl.contingent.sysop.entity.SysopMsg;
import moscow.ptnl.contingent.sysop.entity.SysopMsgParam;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;
import java.util.Map;

@NoRepositoryBean
public interface SysopMsgParamRepository {

    Map<SysopMsg, List<SysopMsgParam>> getSysopMsgParamsBySysopMsgList(List<SysopMsg> sysop);

    List<SysopMsgParam> saveAll(List<SysopMsgParam> sysopMsgParams);
}
