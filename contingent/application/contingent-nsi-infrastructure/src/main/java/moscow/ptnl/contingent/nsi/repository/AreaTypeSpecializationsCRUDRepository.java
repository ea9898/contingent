package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.nsi.domain.area.AreaTypeSpecializations;
import moscow.ptnl.contingent.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation= Propagation.MANDATORY)
public interface AreaTypeSpecializationsCRUDRepository extends PagingAndSortingRepository<AreaTypeSpecializations, Long> {
}
