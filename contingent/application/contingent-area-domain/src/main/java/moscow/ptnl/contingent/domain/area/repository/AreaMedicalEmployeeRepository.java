package moscow.ptnl.contingent.domain.area.repository;

import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface AreaMedicalEmployeeRepository {

    List<AreaMedicalEmployees> getEmployeesByAreaId(long areaId);

    List<AreaMedicalEmployees> getEmployeesMainActualByAreaId(long areaId);

    List<AreaMedicalEmployees> getEmployeesReplacementActualByAreaId(long areaId);

    List<AreaMedicalEmployees> findEmployees(long jobId, Boolean replacement);

    List<Area> findAreasByEmployee(long jobId);

    List<Area> findAreas(List<Long> areaIds, List<Long> jobIds, List<String> snils);

    List<AreaMedicalEmployees> findAllById(List<Long> ids);

    List<AreaMedicalEmployees> saveAll(List<AreaMedicalEmployees> areaMedicalEmployees);

    void delete(AreaMedicalEmployees areaMedicalEmployees);
}
