package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;
import moscow.ptnl.contingent.area.entity.area.MuProfile;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.Area;

import java.util.stream.Collectors;

@Component
public class AreaMapper implements Transform<Area, moscow.ptnl.contingent.area.entity.area.Area> {

    @Autowired
    private AreaTypeShortMapper areaTypeShortMapper;

    @Autowired
    private AreaMedicalEmployeeMapper areaMedicalEmployeeMapper;

    @Override
    public Area entityToDtoTransform(moscow.ptnl.contingent.area.entity.area.Area entityObject) {
        Area area = new Area();
        area.setId(entityObject.getId());
        area.setMoId(entityObject.getMoId());
        area.setMuId(entityObject.getMuId());
        area.setNumber(entityObject.getNumber());
        area.setDescription(entityObject.getDescription());
        area.setAreaType(areaTypeShortMapper.entityToDtoTransform(entityObject.getAreaType()));
        area.setAgeMin(entityObject.getAgeMin());
        area.setAgeMax(entityObject.getAgeMax());
        area.setAgeMinM(entityObject.getAgeMMin());
        area.setAgeMaxM(entityObject.getAgeMMax());
        area.setAgeMinW(entityObject.getAgeWMin());
        area.setAgeMaxW(entityObject.getAgeWMax());
        area.setAutoAssignForAttachment(entityObject.getAutoAssignForAttach());
        area.setAttachByMedicalReason(entityObject.getAttachByMedicalReason());
        area.setArchive(Boolean.TRUE.equals(entityObject.getArchived()));

        if (!entityObject.getActualMedicalEmployees().isEmpty()) {
            Area.MedicalEmployees medicalEmployees = new Area.MedicalEmployees();
            medicalEmployees.getMedicalEmployees().addAll(entityObject.getActualMedicalEmployees().stream()
                    .map(areaMedicalEmployeeMapper::entityToDtoTransform)
                    .collect(Collectors.toList()));
            area.setMedicalEmployees(medicalEmployees);
        }
        if (!entityObject.getPrimaryAreaTypes().isEmpty()) {
            Area.PrimaryAreaTypeCodes areaTypeCodes = new Area.PrimaryAreaTypeCodes();
            areaTypeCodes.getAreaTypes().addAll(entityObject.getPrimaryAreaTypes().stream()
                    .map(AreaToAreaType::getMuProfile)
                    .map(MuProfile::getAreaType)
                    .map(areaTypeShortMapper::entityToDtoTransform)
                    .collect(Collectors.toList()));
            area.setPrimaryAreaTypeCodes(areaTypeCodes);
        }
        return area;
    }

    @Override
    public moscow.ptnl.contingent.area.entity.area.Area dtoToEntityTransform(Area dtoObject) {
        return null;
    }
}
