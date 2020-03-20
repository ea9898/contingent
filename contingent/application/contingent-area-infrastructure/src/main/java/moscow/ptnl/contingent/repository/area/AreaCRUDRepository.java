package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface AreaCRUDRepository extends PagingAndSortingRepository<Area, Long> {
}
