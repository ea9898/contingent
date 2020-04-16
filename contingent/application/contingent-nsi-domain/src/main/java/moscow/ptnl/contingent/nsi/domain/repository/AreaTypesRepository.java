package moscow.ptnl.contingent.nsi.domain.repository;

import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.Optional;

@NoRepositoryBean
public interface AreaTypesRepository {

    Optional<AreaType> findById(Long areaTypeId);
}
