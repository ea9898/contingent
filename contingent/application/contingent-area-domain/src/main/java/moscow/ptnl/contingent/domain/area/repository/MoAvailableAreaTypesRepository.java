package moscow.ptnl.contingent.domain.area.repository;

import moscow.ptnl.contingent.domain.area.entity.MoAvailableAreaTypes;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface MoAvailableAreaTypesRepository {

    List<MoAvailableAreaTypes> findAreaTypes(long moId);

    List<MoAvailableAreaTypes> findByAreaTypes(AreaType areaType, Long moId);

    MoAvailableAreaTypes save(MoAvailableAreaTypes moAvailableAreaTypes);

    void delete(MoAvailableAreaTypes moAvailableAreaTypes);
}
