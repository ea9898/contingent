package moscow.ptnl.contingent.repository;

import moscow.ptnl.contingent.sysop.entity.Sysop;
import moscow.ptnl.contingent.sysop.entity.SysopMsg;
import moscow.ptnl.contingent.sysop.entity.SysopMsg_;
import moscow.ptnl.contingent.sysop.repository.SysopMsgRepository;
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
public class SysopMsgRepositoryImpl extends BaseRepository implements SysopMsgRepository {

    @Autowired
    SysopMsgCRUDRepository sysopMsgCRUDRepository;

    @Override
    public SysopMsg save(SysopMsg sysopMsg) {
        return sysopMsgCRUDRepository.save(sysopMsg);
    }

    @Override
    public SysopMsg getOne(Long sysopId) {
        return sysopMsgCRUDRepository.getOne(sysopId);
    }

    @Override
    public List<SysopMsg> getSysopMsgBySysop(Sysop sysop) {
        Specification<SysopMsg> specification =
            (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.and(
                    criteriaBuilder.equal(root.get(SysopMsg_.sysop.getName()), sysop)
                );

        return sysopMsgCRUDRepository.findAll(specification);
    }

    @Override
    public Map<SysopMsg, List<SysopMsg>> getSysopMsgChildsMap(List<SysopMsg> sysopMsgs) {
        List<Long> sysopMsgsIds = sysopMsgs.stream().map(s -> s.getId()).collect(Collectors.toList());
        Specification<SysopMsg> specification =
                (root, criteriaQuery, criteriaBuilder) ->
                    criteriaBuilder.in(root.get(SysopMsg_.parentMessage.getName()).get(SysopMsg_.id.getName())).value(sysopMsgsIds); //root.get(SysopMsg_.parentMessage.getName()).in(sysopMsgs);

        return sysopMsgCRUDRepository.findAll(specification).stream().collect(Collectors.groupingBy(SysopMsg::getParentMessage));
    }
}
