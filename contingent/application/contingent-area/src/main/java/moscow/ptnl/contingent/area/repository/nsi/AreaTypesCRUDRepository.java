package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.repository.CommonRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface AreaTypesCRUDRepository extends CommonRepository<AreaType, Long> {

    List<AreaType> findAreaTypesByCode(List<Long> codes);
}
