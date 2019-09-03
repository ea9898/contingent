package moscow.ptnl.contingent.nsi.repository;

import org.springframework.data.repository.NoRepositoryBean;

import java.util.Optional;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeRelations;

@NoRepositoryBean
public interface AreaTypeRelationsRepository {

    Optional<AreaTypeRelations> getByDependentAndPrimaryAreaTypes(AreaType dependentAreaType, AreaType primaryAreaType);
}