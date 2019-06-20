package moscow.ptnl.contingent.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.AreaTypeMedicalPositions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
@Transactional(propagation = Propagation.MANDATORY)
public interface AreaTypeMedicalPositionsRepository {

    List<AreaTypeMedicalPositions> getPositionsByAreaType(long areaTypeId);
}
