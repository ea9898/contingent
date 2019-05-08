package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployee;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean

public interface AreaMedicalEmployeeRepository {

    boolean isOtherMainEmployeeExist(long areaId, List<Long> deleteIds);

    List<AreaMedicalEmployee> getMainEmployees(long areaId, List<Long> deleteIds);

    List<Long> getJobIds(long areaId, List<Long> deleteIds);

    List<AreaMedicalEmployee> getEmployeesByJobInfoId(long medicalEmployeeJobInfoId, List<Long> deleteIds);
}
