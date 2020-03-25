package moscow.ptnl.contingent.domain.area.repository;

import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaToAreaType;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface AreaToAreaTypeRepository {

	List<AreaToAreaType> getAreaTypesByAreaId(long areaId);

    List<AreaToAreaType> findAreaTypesByAreaAndTypeCode(Area area, List<Long> areaTypes);

    AreaToAreaType save(AreaToAreaType areaToAreaType);

    void deleteAll(List<AreaToAreaType> areaToAreaTypes);
}
