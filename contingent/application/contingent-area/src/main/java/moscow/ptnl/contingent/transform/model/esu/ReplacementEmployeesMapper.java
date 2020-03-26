package moscow.ptnl.contingent.transform.model.esu;

import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;
import moscow.ptnl.contingent.transform.Transform;
import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import org.springframework.stereotype.Component;

import java.util.Set;
import java.util.stream.Collectors;

@Component
public class ReplacementEmployeesMapper implements Transform<AreaInfoEvent.ReplacementEmployees, Set<AreaMedicalEmployees>> {

    @Override
    public AreaInfoEvent.ReplacementEmployees entityToDtoTransform(Set<AreaMedicalEmployees> entity) {
        AreaInfoEvent.ReplacementEmployees employees = new AreaInfoEvent.ReplacementEmployees();
        employees.getIdmr().addAll(entity.stream().map(AreaMedicalEmployees::getMedicalEmployeeJobId).collect(Collectors.toList()));

        return employees;
    }

    @Override
    public Set<AreaMedicalEmployees> dtoToEntityTransform(AreaInfoEvent.ReplacementEmployees dtoObject) {
        return null;
    }
}
