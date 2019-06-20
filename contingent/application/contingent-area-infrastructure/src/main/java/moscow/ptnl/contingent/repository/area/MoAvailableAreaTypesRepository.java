package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface MoAvailableAreaTypesRepository {

    List<MoAvailableAreaTypes> findAreaTypes(long moId);

    List<MoAvailableAreaTypes> findByAreaTypes(AreaType areaType);
}
