package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.Area;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface AreaRepository {

	List<Area> findAreas(Long moId, Long muId, Long areaTypeCode, Integer number, Boolean actual);

	List<Area> findAreas(Long moId, Long muId, List<Long> areaTypeCodes, Integer number, Boolean actual);

    List<Area> findAreasWithMuIdNull(Long moId, Long areaTypeCode, Integer number, Boolean actual);

    List<Area> findDependentAreasByAreaEqAreaType(Area area);

    List<Area> findPrimaryAreasByAreaEqAreaType(Area area);

    Long getNextAreaId();
}
