package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.nsi.domain.area.AreaTypeKind;
import moscow.ptnl.contingent.repository.CommonRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface AreaTypesKindCRUDRepository extends CommonRepository<AreaTypeKind, Long> {
}
