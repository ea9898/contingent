package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.Area;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface AreaRepository {

	List<Area> findAreas(Long moId, Long muId, String areaTypeCode, Integer number, Boolean actual);

	List<Area> findAreas(Long moId, Long muId, List<String> areaTypeCodes, Integer number, Boolean actual);
}
