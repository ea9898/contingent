package moscow.ptnl.contingent.area.transform;

import java.util.Optional;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployees;
import moscow.ptnl.contingent.area.entity.nsi.PositionNom;
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

    @Override
    public MedicalEmployee entityToDtoTransform(AreaMedicalEmployees entityObject) {
        MedicalEmployee employee = new MedicalEmployee();
        employee.setId(entityObject.getId());
        employee.setMedicalEmployeeJobId(entityObject.getMedicalEmployeeJobId());
        employee.setSnils(entityObject.getSnils());
        employee.setIsReplacement(entityObject.getReplacement());
        employee.setStartDate(entityObject.getStartDate());
        employee.setEndDate(entityObject.getEndDate());

        if (entityObject.getPositionNomCode() != null) {
            Optional<PositionNom> result = positionNomRepository.getByCode(entityObject.getPositionNomCode());
            if (result.isPresent()) {
                employee.setPosition(positionNomMapper.entityToDtoTransform(result.get()));
            }
        }
        return employee;
    }

    @Override
    public AreaMedicalEmployees dtoToEntityTransform(MedicalEmployee dtoObject) {
        return null;
    }
}
