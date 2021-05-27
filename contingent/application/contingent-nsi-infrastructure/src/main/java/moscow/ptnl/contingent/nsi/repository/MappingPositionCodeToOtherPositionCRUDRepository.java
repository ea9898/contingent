package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.nsi.domain.area.MappingPositionCodeToOtherPosition;
import moscow.ptnl.contingent.repository.CommonRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface MappingPositionCodeToOtherPositionCRUDRepository extends CommonRepository<MappingPositionCodeToOtherPosition, Long> {

    List<MappingPositionCodeToOtherPosition> findByPsGlobalId(Long psGlobalId);
}
