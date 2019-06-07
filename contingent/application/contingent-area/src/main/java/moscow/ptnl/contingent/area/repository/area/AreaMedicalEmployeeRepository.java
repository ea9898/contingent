package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployees;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface AreaMedicalEmployeeRepository {

    List<AreaMedicalEmployees> getEmployeesByAreaId(long areaId);

    List<AreaMedicalEmployees> getEmployeesMainActualByAreaId(long areaId);

    List<AreaMedicalEmployees> getEmployeesReplacementActualByAreaId(long areaId);
}
