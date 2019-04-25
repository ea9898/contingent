package moscow.ptnl.contingent.area.transform;

import org.springframework.stereotype.Component;
import ru.gov.emias2.contingent.v1.area.types.MedicalEmployee;

@Component
public class AreaMedicalEmployeeMapper implements Transform<MedicalEmployee, moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployee> {

    @Override
    public MedicalEmployee entityToDtoTransform(moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployee entityObject) {
        MedicalEmployee employee = new MedicalEmployee();
        //Todo разобраться где брать ИД
//        employee.setId(entityObject.getId());
        employee.setMedicalEmployeeJobInfoId(entityObject.getMedicalEmployeeJobInfoId());
        employee.setPositionId(entityObject.getMedicalPositionId());
        employee.setSnils(entityObject.getSnils());
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
