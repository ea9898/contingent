package moscow.ptnl.contingent.domain.area.repository;

import moscow.ptnl.contingent.domain.area.entity.Area;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;
import java.util.Optional;
import javax.persistence.EntityManager;

@NoRepositoryBean
public interface AreaRepository {

    List<Area> findAreas(Long moId, Long muId, Long areaTypeCode, Integer number, Boolean actual);

    List<Area> findAreas(Long moId, Long muId, List<Long> areaTypeCodes, Integer number, Boolean actual);

    List<Area> findAreas(Long areaTypeClassCode, Long moId, List<Long> muIds, List<Long> areaTypeCodes, Integer number,
                         String description, Boolean archived);

    List<Area> findAreas(Long areaTypeKindCode, Boolean archived, Long medicalEmployeeJobInfo);

    List<Area> findDependentAreasByMuMoIdAndType(Long muId, Long moId, Long areaTypeCode, Long areaTypeKindCode);

    List<Area> findPrimaryAreasByAreaEqAreaType(Area area);

    Page<Area> findAreas(Long moId, List<Long> muIds, List<Long> areaTypeCodes, List<Long> specializationCodes, List<Long> areaIds, PageRequest paging);

    Long getNextAreaId();
    
    EntityManager getEntityManager();

    List<Area> findAllById(List<Long> areaIds);

    Optional<Area> findById(Long areaId);

    Area save(Area area);

}