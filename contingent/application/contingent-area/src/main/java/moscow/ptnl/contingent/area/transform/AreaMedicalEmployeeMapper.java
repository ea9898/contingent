package moscow.ptnl.contingent.area.transform;

import org.springframework.stereotype.Component;
import ru.gov.emias2.contingent.v1.area.types.MedicalEmployee;

@Component
public class AreaMedicalEmployeeMapper implements Transform<MedicalEmployee, moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployee> {

    @Override
    public MedicalEmployee entityToDtoTransform(moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployee entityObject) {
        MedicalEmployee employee = new MedicalEmployee();
        employee.setId(entityObject.getMedicalEmployeeId());
        employee.setMedicalEmployeeJobInfoId(entityObject.getMedicalEmployeeJobInfoId());
        employee.setPositionId(entityObject.getMedicalPositionId());
        //Todo разобраться где взять СИЛС
//        employee.setSnils(entityObject.getNumber());
        employee.setIsReplacement(entityObject.getReplacement());
        employee.setStartDate(entityObject.getStartDate());
        employee.setEndDate(entityObject.getEndDate());

        return employee;
    }

    @Override
    public moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployee dtoToEntityTransform(MedicalEmployee dtoObject) {
        return null;
    }
}
