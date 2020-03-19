package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.area.AreaAddress;
import moscow.ptnl.contingent.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface AreaAddressPagingAndSortingRepository extends PagingAndSortingRepository<AreaAddress, Long> {
}
