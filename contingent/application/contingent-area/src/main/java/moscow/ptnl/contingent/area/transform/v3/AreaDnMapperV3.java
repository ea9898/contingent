package moscow.ptnl.contingent.area.transform.v3;

import moscow.ptnl.contingent.area.transform.v3.AreaTypeShortMapperV3;
import moscow.ptnl.contingent.area.transform.v3.CodeNameTypeMapperV3;
import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;
import moscow.ptnl.contingent.domain.area.entity.AreaMuService;
import moscow.ptnl.contingent.domain.area.model.area.AreaInfo;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeSpecializations;
import moscow.ptnl.contingent.nsi.domain.area.PositionCode;
import moscow.ptnl.contingent.nsi.domain.repository.PositionCodeRepository;
import moscow.ptnl.contingent.nsi.domain.repository.SpecializationRepository;
import moscow.ptnl.contingent.transform.Transform;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.v3.AreaDn;
import ru.mos.emias.contingent2.core.v3.AreaTypeProfile;
import ru.mos.emias.contingent2.core.v3.MuService;
import ru.mos.emias.contingent2.core.v3.PositionNomClinic;
import ru.mos.emias.contingent2.core.v3.Specialization;

import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Component
public class AreaDnMapperV3 implements Transform<AreaDn, AreaInfo> {

    @Autowired
    private AreaTypeShortMapperV3 areaTypeShortMapper;

    @Autowired
    private SpecializationRepository specializationRepository;

    @Autowired
    private PositionCodeRepository positionCodeRepository;

    @Autowired
    private CodeNameTypeMapperV3 codeNameTypeMapper;

    @Override
    public AreaDn entityToDtoTransform(AreaInfo entityObject) {
        moscow.ptnl.contingent.domain.area.entity.Area areaObject = entityObject.getArea();
        AreaDn area = new AreaDn();
        area.setId(areaObject.getId());
        area.setMoId(areaObject.getMoId());
        area.setMuId(areaObject.getMuId());
        area.setNumber(areaObject.getNumber());
        area.setAreaType(areaTypeShortMapper.entityToDtoTransform(areaObject.getAreaType()));
        area.setSpecializations(new AreaDn.Specializations());
        area.getSpecializations().getSpecializations().addAll(areaObject.getAreaType().getAreaTypeSpecializations().stream()
                .map(this::mapSpecialization)
                .filter(Objects::nonNull)
                .collect(Collectors.toList())
        );
        area.setDateCreated(areaObject.getCreateDate().toLocalDate());
        if (entityObject.getAreaServicedMUs() != null && !entityObject.getAreaServicedMUs().isEmpty()) {
            MuService muService = new MuService();
            muService.getMuIds().addAll(entityObject.getAreaServicedMUs().stream().map(AreaMuService::getMuId).collect(Collectors.toList()));
            area.setMuService(muService);
        }
        if (entityObject.getMainAreaMedicalEmployees() != null) {
            entityObject.getMainAreaMedicalEmployees().stream()
                    .filter(me -> me.getError() == null || !me.getError())
                    .min((o1, o2) -> Objects.compare(o1.getMedicalEmployeeJobId(), o2.getMedicalEmployeeJobId(), Long::compare))
                    .map(this::entityToDtoTransform)
                    .ifPresent(area::setMedicalEmployee);
        }
        return area;
    }

    private AreaDn.MedicalEmployee entityToDtoTransform(AreaMedicalEmployees entityObject) {
        AreaDn.MedicalEmployee employee = new AreaDn.MedicalEmployee();
        employee.setId(entityObject.getId());
        employee.setMedicalEmployeeJobId(entityObject.getMedicalEmployeeJobId());
        employee.setSnils(entityObject.getSnils());
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

    private Specialization mapSpecialization(AreaTypeSpecializations entity) {
        moscow.ptnl.contingent.nsi.domain.area.Specialization specializationEntity = specializationRepository.getByCode(entity.getSpecializationCode());

        if (specializationEntity == null) {
            return null;
        }
        Specialization specialization = new Specialization();
        specialization.setCode(specializationEntity.getCode());
        specialization.setName(specializationEntity.getTitle());

        return specialization;
    }

    @Override
    public AreaInfo dtoToEntityTransform(AreaDn dtoObject) {
        return null;
    }
}
