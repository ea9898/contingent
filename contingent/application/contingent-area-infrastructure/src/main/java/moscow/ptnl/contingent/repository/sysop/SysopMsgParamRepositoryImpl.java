package moscow.ptnl.contingent.repository.sysop;

import moscow.ptnl.contingent.area.entity.sysop.Sysop;
import moscow.ptnl.contingent.area.entity.sysop.SysopMsg;
import moscow.ptnl.contingent.area.entity.sysop.SysopMsgParam;
import moscow.ptnl.contingent.area.entity.sysop.SysopMsgParam_;
import moscow.ptnl.contingent.area.entity.sysop.SysopMsg_;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class SysopMsgParamRepositoryImpl extends BaseRepository implements SysopMsgParamRepository {

    @Autowired
    SysopMsgParamCRUDRepository sysopMsgParamCRUDRepository;

    @Override
    public Map<SysopMsg, List<SysopMsgParam>> getSysopMsgParamsBySysopMsgList(List<SysopMsg> sysop) {
        Specification<SysopMsgParam> specification =
                (root, criteriaQuery, criteriaBuilder) ->
                        root.get(SysopMsgParam_.sysopMsg.getName()).in(sysop);

        return sysopMsgParamCRUDRepository.findAll(specification).stream().collect(Collectors.groupingBy(SysopMsgParam::getSysopMsg));
    }
}
