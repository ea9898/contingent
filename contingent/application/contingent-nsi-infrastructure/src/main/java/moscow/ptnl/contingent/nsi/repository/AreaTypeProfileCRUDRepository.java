package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.nsi.domain.area.AreaTypeProfile;
import moscow.ptnl.contingent.repository.CommonRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface AreaTypeProfileCRUDRepository extends CommonRepository<AreaTypeProfile, Long> {

    Optional<AreaTypeProfile> findByCodeAndAreaTypeCode(Long areaTypeProfileCode, Long areaTypeCode);
}
