package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface AreaToAreaTypeRepository {

	List<AreaToAreaType> getAreaTypesByAreaId(long areaId);
}
