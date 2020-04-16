package moscow.ptnl.contingent.nsi.domain.repository;

import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeMedicalPositions;

@Repository
@Transactional(propagation = Propagation.MANDATORY)
public interface AreaTypeMedicalPositionsRepository {

    List<AreaTypeMedicalPositions> getPositionsByAreaType(long areaTypeId);
}
