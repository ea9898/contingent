package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.nsi.domain.area.PositionCode;
import moscow.ptnl.contingent.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Set;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface PositionCodeCRUDRepository extends PagingAndSortingRepository<PositionCode, String> {

    List<PositionCode> findByGlobalIdIn(Set<Long> globalIds);
}
