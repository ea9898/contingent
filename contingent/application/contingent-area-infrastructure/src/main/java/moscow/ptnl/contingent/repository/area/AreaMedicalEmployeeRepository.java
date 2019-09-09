package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployees;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface AreaMedicalEmployeeRepository {

    List<AreaMedicalEmployees> getEmployeesByAreaId(long areaId);

    List<AreaMedicalEmployees> getEmployeesMainActualByAreaId(long areaId);

    List<AreaMedicalEmployees> getEmployeesReplacementActualByAreaId(long areaId);

    List<AreaMedicalEmployees> findEmployees(long jobId, Boolean replacement);

    List<Area> findAreas(List<Long> areaIds, List<Long> jobIds, List<String> snils);
}
