package moscow.ptnl.contingent.area.transform.v3;

import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;
import moscow.ptnl.contingent.nsi.domain.area.PositionCode;
import moscow.ptnl.contingent.nsi.domain.repository.PositionCodeRepository;
import moscow.ptnl.contingent.transform.Transform;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.v3.MedicalEmployee;
import ru.mos.emias.contingent2.core.v3.PositionNomClinic;

import java.util.Optional;

@Component
public class AreaMedicalEmployeeMapperV3 implements Transform<MedicalEmployee, AreaMedicalEmployees> {

    @Autowired
    private PositionCodeRepository positionCodeRepository;

    @Override
    public MedicalEmployee entityToDtoTransform(AreaMedicalEmployees entityObject) {
        MedicalEmployee employee = new MedicalEmployee();
        employee.setId(entityObject.getId());
        employee.setMedicalEmployeeJobId(entityObject.getMedicalEmployeeJobId());
        employee.setSnils(entityObject.getSnils());
        employee.setTempDutyStartDate(entityObject.getTempDutyStartDate());
        employee.setIsReplacement(entityObject.getReplacement());
        employee.setStartDate(entityObject.getStartDate());
        employee.setEndDate(entityObject.getEndDate());

        if (entityObject.getPositionCode() != null) {
            PositionNomClinic positionNomClinic = new PositionNomClinic();
            positionNomClinic.setCode(entityObject.getPositionCode());
            Optional<PositionCode> positionCodeOptional = positionCodeRepository.getByCode(entityObject.getPositionCode());
            positionCodeOptional.ifPresent(positionCode -> positionNomClinic.setName(positionCode.getConstantTitle()));
            employee.setPosition(positionNomClinic);
        }
        return employee;
    }

    @Override
    public AreaMedicalEmployees dtoToEntityTransform(MedicalEmployee dtoObject) {
        return null;
    }
}
