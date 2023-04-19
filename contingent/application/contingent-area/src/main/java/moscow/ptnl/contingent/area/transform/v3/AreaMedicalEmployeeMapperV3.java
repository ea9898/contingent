package moscow.ptnl.contingent.area.transform.v3;

import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;
import moscow.ptnl.contingent.nsi.domain.repository.PositionCodeRepository;
import moscow.ptnl.contingent.nsi.repository.PositionSuppCRUDRepository;
import moscow.ptnl.contingent.transform.Transform;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import ru.mos.emias.contingent2.core.v3.MedicalEmployee;
import ru.mos.emias.contingent2.core.v3.PositionNomClinic;

@Component
public class AreaMedicalEmployeeMapperV3 implements Transform<MedicalEmployee, AreaMedicalEmployees> {

    @Autowired
    private PositionCodeRepository positionCodeRepository;

    @Autowired
    private PositionSuppCRUDRepository positionSuppCRUDRepository;

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

        if (entityObject.getPositionCode() != null ) {
            positionCodeRepository.getByCode(entityObject.getPositionCode()).ifPresent(position -> {
                PositionNomClinic positionNomClinic = new PositionNomClinic();
                positionNomClinic.setCode(entityObject.getPositionCode());
                positionNomClinic.setName(position.getConstantTitle());
                employee.setPosition(positionNomClinic);
            });
        }
        if (employee.getPosition() == null && entityObject.getPositionCodeSupp() != null) {
            positionSuppCRUDRepository.findByCode(String.valueOf(entityObject.getPositionCodeSupp())).ifPresent(position -> {
                PositionNomClinic positionNomClinic = new PositionNomClinic();
                positionNomClinic.setCode(position.getCode());
                positionNomClinic.setName(position.getTitleShort());
                employee.setPosition(positionNomClinic);
            });
        }
        return employee;
    }

    @Override
    public AreaMedicalEmployees dtoToEntityTransform(MedicalEmployee dtoObject) {
        return null;
    }
}
