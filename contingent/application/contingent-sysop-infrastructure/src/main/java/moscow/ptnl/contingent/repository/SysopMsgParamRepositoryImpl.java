package moscow.ptnl.contingent.repository;

import moscow.ptnl.contingent.sysop.entity.SysopMsg;
import moscow.ptnl.contingent.sysop.entity.SysopMsgParam;
import moscow.ptnl.contingent.sysop.entity.SysopMsgParam_;
import moscow.ptnl.contingent.sysop.repository.SysopMsgParamRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;
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

    @Override
    public List<SysopMsgParam> saveAll(List<SysopMsgParam> sysopMsgParams) {
        return sysopMsgParamCRUDRepository.saveAll(sysopMsgParams);
    }
}
