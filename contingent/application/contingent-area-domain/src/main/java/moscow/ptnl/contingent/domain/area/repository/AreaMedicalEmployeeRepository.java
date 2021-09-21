package moscow.ptnl.contingent.domain.area.repository;

import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;
import moscow.ptnl.contingent.domain.area.model.area.AreaHistory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.NoRepositoryBean;

import javax.persistence.Tuple;
import java.util.List;
import java.util.Map;

@NoRepositoryBean
public interface AreaMedicalEmployeeRepository {

    List<AreaMedicalEmployees> getEmployeesByAreaId(long areaId);

    List<AreaMedicalEmployees> getEmployeesMainActualByAreaId(long areaId);

    List<AreaMedicalEmployees> getEmployeesReplacementActualByAreaId(long areaId);

    Map<Area, List<AreaMedicalEmployees>> getEmployeesByAreaIds(List<Area> areas);

    List<AreaMedicalEmployees> findEmployees(long jobId, Boolean replacement);

    List<Area> findAreasByEmployee(long jobId);

    List<Area> findAreas(List<Long> areaIds, List<Long> jobIds, List<String> snils);

    List<AreaMedicalEmployees> findAllById(List<Long> ids);

    List<AreaMedicalEmployees> saveAll(List<AreaMedicalEmployees> areaMedicalEmployees);

    void delete(AreaMedicalEmployees areaMedicalEmployees);

    Page<AreaHistory.Event> areaHistory(Long areaId, Pageable paging);
}
