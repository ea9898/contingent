package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployee;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface AreaMedicalEmployeeRepository {

    List<AreaMedicalEmployee> getMainEmployees(long areaId);

    List<AreaMedicalEmployee> getReplacementEmployees(long areaId);

    List<AreaMedicalEmployee> getEmployeesByAreaId(long areaId);
}
