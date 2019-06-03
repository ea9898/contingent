package moscow.ptnl.contingent.area.transform;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.MedicalEmployee;

@Component
public class AreaMedicalEmployeeMapper implements Transform<MedicalEmployee, moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployee> {

    @Autowired
    private PositionNomClinicMapper positionNomClinicMapper;

    @Override
    public MedicalEmployee entityToDtoTransform(moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployee entityObject) {
        MedicalEmployee employee = new MedicalEmployee();
        employee.setId(entityObject.getId());
        employee.setMedicalEmployeeJobInfoId(entityObject.getMedicalEmployeeJobInfoId());
        employee.setSnils(entityObject.getSnils());
        employee.setIsReplacement(entityObject.getReplacement());
        employee.setStartDate(entityObject.getStartDate());
        employee.setEndDate(entityObject.getEndDate());

        if (entityObject.getPositionNomClinic() != null) {
            employee.setPosition(positionNomClinicMapper.entityToDtoTransform(entityObject.getPositionNomClinic()));
        }
        return employee;
    }

    @Override
    public moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployee dtoToEntityTransform(MedicalEmployee dtoObject) {
        return null;
    }
}
