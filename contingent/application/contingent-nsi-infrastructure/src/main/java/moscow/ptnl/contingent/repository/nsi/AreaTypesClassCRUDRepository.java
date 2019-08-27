package moscow.ptnl.contingent.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.AreaTypeClass;
import moscow.ptnl.contingent.repository.CommonRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface AreaTypesClassCRUDRepository extends CommonRepository<AreaTypeClass, Long> {
}
