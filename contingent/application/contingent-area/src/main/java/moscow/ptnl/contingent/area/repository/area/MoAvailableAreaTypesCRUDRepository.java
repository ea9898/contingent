package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.repository.CommonRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface MoAvailableAreaTypesCRUDRepository extends CommonRepository<MoAvailableAreaTypes, Long> {
}
