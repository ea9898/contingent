package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeSpecializations;
import moscow.ptnl.contingent.nsi.repository.SpecializationCRUDRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.AreaDn;
import ru.mos.emias.contingent2.core.Specialization;

import java.util.Objects;
import java.util.stream.Collectors;

@Component
public class AreaDnMapper implements Transform<AreaDn, Area> {

    @Autowired
    private AreaTypeShortMapper areaTypeShortMapper;

    @Autowired
    private SpecializationCRUDRepository specializationCRUDRepository;

    @Autowired
    private AreaDnMedicalEmployeeMapper areaDnMedicalEmployeeMapper;

    @Override
    public AreaDn entityToDtoTransform(Area entityObject) {
        AreaDn area = new AreaDn();
        area.setId(entityObject.getId());
        area.setMoId(entityObject.getMoId());
        area.setMuId(entityObject.getMuId());
        area.setNumber(entityObject.getNumber());
        area.setAreaType(areaTypeShortMapper.entityToDtoTransform(entityObject.getAreaType()));
        area.setSpecializations(new AreaDn.Specializations());
        area.getSpecializations().getSpecializations().addAll(entityObject.getAreaType().getAreaTypeSpecializations().stream()
                .map(this::mapSpecialization)
                .filter(Objects::nonNull)
                .collect(Collectors.toList())
        );
        if (entityObject.getActualMainMedicalEmployees() != null) {
            entityObject.getActualMainMedicalEmployees().stream()
                    .min((o1, o2) -> Objects.compare(o1.getMedicalEmployeeJobId(), o2.getMedicalEmployeeJobId(), Long::compare))
                    .map(areaDnMedicalEmployeeMapper::entityToDtoTransform)
                    .ifPresent(area::setMedicalEmployee);
        }
        return area;
    }

    private Specialization mapSpecialization(AreaTypeSpecializations entity) {
        moscow.ptnl.contingent.nsi.domain.area.Specialization specializationEntity = specializationCRUDRepository.getByCode(entity.getSpecializationCode());

        if (specializationEntity == null) {
            return null;
        }
        Specialization specialization = new Specialization();
        specialization.setCode(specializationEntity.getCode());
        specialization.setName(specializationEntity.getTitle());

        return specialization;
    }

    @Override
    public Area dtoToEntityTransform(AreaDn dtoObject) {
        return null;
    }
}
