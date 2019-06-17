package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.MuAvailableAreaTypes;
import moscow.ptnl.contingent.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface MuAvailableAreaTypesCRUDRepository extends PagingAndSortingRepository<MuAvailableAreaTypes, Long> {
}
