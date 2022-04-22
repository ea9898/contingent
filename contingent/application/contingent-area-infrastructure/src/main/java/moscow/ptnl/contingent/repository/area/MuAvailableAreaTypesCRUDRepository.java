package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.MuAvailableAreaTypes;
import moscow.ptnl.contingent.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface MuAvailableAreaTypesCRUDRepository extends PagingAndSortingRepository<MuAvailableAreaTypes, Long> {

    List<MuAvailableAreaTypes> findByMoIdIn(List<Long> moIds);
}
