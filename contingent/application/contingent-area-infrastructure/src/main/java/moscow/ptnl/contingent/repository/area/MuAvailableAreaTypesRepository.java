package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.MuAvailableAreaTypes;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface MuAvailableAreaTypesRepository {

    List<MuAvailableAreaTypes> findAreaTypes(long muId);

    List<MuAvailableAreaTypes> findByAreaTypes(AreaType areaType, Long muId);
}
