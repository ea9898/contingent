package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployees;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.MedicalEmployee;

@Component
public class AreaMedicalEmployeeMapper implements Transform<MedicalEmployee, AreaMedicalEmployees> {

    @Autowired
    private PositionNomClinicMapper positionNomClinicMapper;

    @Override
    public MedicalEmployee entityToDtoTransform(AreaMedicalEmployees entityObject) {
        MedicalEmployee employee = new MedicalEmployee();
        employee.setId(entityObject.getId());
        employee.setMedicalEmployeeJobInfoId(entityObject.getMedicalEmployeeJobInfoId());
        employee.setSnils(entityObject.getSnils());
        employee.setIsReplacement(entityObject.getReplacement());
        employee.setStartDate(entityObject.getStartDate());
        employee.setEndDate(entityObject.getEndDate());

        if (entityObject.getPositionNom() != null) {
            employee.setPosition(positionNomClinicMapper.entityToDtoTransform(entityObject.getPositionNom()));
        }
        return employee;
    }

    @Override
    public AreaMedicalEmployees dtoToEntityTransform(MedicalEmployee dtoObject) {
        return null;
    }
}
