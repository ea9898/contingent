package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeRelations;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.Optional;

@NoRepositoryBean
public interface AreaTypeRelationsRepository {

    Optional<AreaTypeRelations> getByDependentAndPrimaryAreaTypes(AreaType dependentAreaType, AreaType primaryAreaType);
}