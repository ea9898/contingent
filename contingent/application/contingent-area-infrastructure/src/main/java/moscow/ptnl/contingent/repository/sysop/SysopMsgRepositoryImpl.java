package moscow.ptnl.contingent.repository.sysop;

import moscow.ptnl.contingent.area.entity.area.AreaAddress;
import moscow.ptnl.contingent.area.entity.area.AreaAddress_;
import moscow.ptnl.contingent.area.entity.area.Area_;
import moscow.ptnl.contingent.area.entity.area.MoAddress_;
import moscow.ptnl.contingent.area.entity.sysop.Sysop;
import moscow.ptnl.contingent.area.entity.sysop.SysopMsg;
import moscow.ptnl.contingent.area.entity.sysop.SysopMsg_;
import moscow.ptnl.contingent.nsi.domain.area.AreaType_;
import moscow.ptnl.contingent.repository.BaseRepository;
import moscow.ptnl.contingent.repository.area.AreaAddressPagingAndSortingRepository;
import moscow.ptnl.contingent.repository.area.AreaAddressRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class SysopMsgRepositoryImpl extends BaseRepository implements SysopMsgRepository {

    @Autowired
    SysopMsgCRUDRepository sysopMsgCRUDRepository;

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
        Specification<SysopMsg> specification =
                (root, criteriaQuery, criteriaBuilder) ->
                        root.get(SysopMsg_.parentMessage.getName()).in(sysopMsgs);

        return sysopMsgCRUDRepository.findAll(specification).stream().collect(Collectors.groupingBy(SysopMsg::getParentMessage));
    }
}
