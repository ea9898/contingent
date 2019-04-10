package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.Area;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface AreaRepository {

	List<Area> findAreas(long muId, String areaTypeCode, Boolean actual);
}
