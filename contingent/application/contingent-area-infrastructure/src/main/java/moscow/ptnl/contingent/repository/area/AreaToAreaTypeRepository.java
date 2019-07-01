package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface AreaToAreaTypeRepository {

	List<AreaToAreaType> getAreaTypesByAreaId(long areaId);

    List<AreaToAreaType> findAreaTypesByAreaAndTypeCode(Area area, List<Long> areaTypes);
}
