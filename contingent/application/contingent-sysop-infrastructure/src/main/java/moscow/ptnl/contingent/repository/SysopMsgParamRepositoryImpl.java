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
import moscow.ptnl.contingent.sysop.entity.SysopMsg_;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class SysopMsgParamRepositoryImpl extends BaseRepository implements SysopMsgParamRepository {

    @Autowired
    SysopMsgParamCRUDRepository sysopMsgParamCRUDRepository;

    @Override
    public Map<SysopMsg, List<SysopMsgParam>> getSysopMsgParamsBySysopMsgList(List<SysopMsg> sysop) {
        List<Long> sysopIds = sysop.stream().map(s -> s.getId()).collect(Collectors.toList());
        Specification<SysopMsgParam> specification =
                (root, criteriaQuery, criteriaBuilder) ->
                    criteriaBuilder.in(root.get(SysopMsgParam_.sysopMsg.getName()).get(SysopMsg_.id.getName())).value(sysopIds); //root.get(SysopMsgParam_.sysopMsg.getName()).in(sysop);

        return sysopMsgParamCRUDRepository.findAll(specification).stream().collect(Collectors.groupingBy(SysopMsgParam::getSysopMsg));
    }

    @Override
    public List<SysopMsgParam> saveAll(List<SysopMsgParam> sysopMsgParams) {
        return sysopMsgParamCRUDRepository.saveAll(sysopMsgParams);
    }
}
