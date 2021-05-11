package moscow.ptnl.contingent.nsi.domain.repository;

import moscow.ptnl.contingent.nsi.domain.area.AreaTypeProfile;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.Optional;

@NoRepositoryBean
public interface AreaTypeProfileRepository {

    Optional<AreaTypeProfile> findByCodeAndAreaType(Long areaTypeProfileCode, Long areaTypeCode);
}
