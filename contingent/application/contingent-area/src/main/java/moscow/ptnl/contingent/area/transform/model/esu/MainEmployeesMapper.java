package moscow.ptnl.contingent.area.transform.model.esu;

import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;
import moscow.ptnl.contingent.area.transform.Transform;
import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import org.springframework.stereotype.Component;

import java.util.Set;
import java.util.stream.Collectors;

@Component
public class MainEmployeesMapper implements Transform<AreaInfoEvent.MainEmployees, Set<AreaMedicalEmployees>> {

    @Override
    public AreaInfoEvent.MainEmployees entityToDtoTransform(Set<AreaMedicalEmployees> entity) {
        AreaInfoEvent.MainEmployees employees = new AreaInfoEvent.MainEmployees();
        employees.getIdmr().addAll(entity.stream().map(AreaMedicalEmployees::getMedicalEmployeeJobId).collect(Collectors.toList()));

        return employees;
    }

    @Override
    public Set<AreaMedicalEmployees> dtoToEntityTransform(AreaInfoEvent.MainEmployees dtoObject) {
        return null;
    }
}
