package moscow.ptnl.contingent.area.transform;

import java.util.Optional;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployees;
import moscow.ptnl.contingent.area.entity.nsi.PositionCode;
import moscow.ptnl.contingent.area.entity.nsi.PositionNom;
import moscow.ptnl.contingent.area.service.AreaServiceHelper;
import moscow.ptnl.contingent.repository.nsi.PositionCodeRepository;
import moscow.ptnl.contingent.repository.nsi.PositionNomRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.MedicalEmployee;

@Component
public class AreaMedicalEmployeeMapper implements Transform<MedicalEmployee, AreaMedicalEmployees> {

    @Autowired
    private PositionNomMapper positionNomMapper;
    
    @Autowired
    private PositionNomRepository positionNomRepository;

    @Autowired
    private PositionCodeRepository positionCodeRepository;

    @Autowired
    private AreaServiceHelper areaServiceHelper;

    @Override
    public MedicalEmployee entityToDtoTransform(AreaMedicalEmployees entityObject) {
        MedicalEmployee employee = new MedicalEmployee();
        employee.setId(entityObject.getId());
        employee.setMedicalEmployeeJobId(entityObject.getMedicalEmployeeJobId());
        employee.setSnils(entityObject.getSnils());
        employee.setIsReplacement(entityObject.getReplacement());
        employee.setStartDate(entityObject.getStartDate());
        employee.setEndDate(entityObject.getEndDate());

        if (entityObject.getPositionCode() != null) {
            employee.setPosition(positionNomMapper.entityToDtoTransform(areaServiceHelper.getPositionNomByPositionCode(entityObject.getPositionCode())));
        }
        return employee;
    }

    @Override
    public AreaMedicalEmployees dtoToEntityTransform(MedicalEmployee dtoObject) {
        return null;
    }
}
