package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.Area;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;
import javax.persistence.EntityManager;

@NoRepositoryBean
public interface AreaRepository {

    List<Area> findAreas(Long moId, Long muId, Long areaTypeCode, Integer number, Boolean actual);

    List<Area> findAreasWithNotAreaTypeKindCode(Long moId, Long muId, Long areaTypeCode, Long areaTypeKindCode, Integer number, Boolean actual);

    List<Area> findAreas(Long moId, Long muId, List<Long> areaTypeCodes, Integer number, Boolean actual);
    List<Area> findAreas(Long areaTypeClassCode, Long moId, List<Long> muIds, List<Long> areaTypeCodes, Integer number,
                         String description, Boolean archived);

    List<Area> findAreasWithMuIdNullAndNotAreaTypeKindCode(Long moId, Long areaTypeCode, Long areaTypeKindCode, Integer number, Boolean actual);

    List<Area> findDependentAreasByMuMoIdAndType(Long muId, Long moId, Long areaTypeCode, Long areaTypeKindCode);

    List<Area> findDependentAreasByAreaEqAreaType(Area area);

    List<Area> findPrimaryAreasByAreaEqAreaType(Area area);

    Long getNextAreaId();
    
    EntityManager getEntityManager();
}
