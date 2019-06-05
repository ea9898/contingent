package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.Area;
import ru.mos.emias.contingent2.core.AreaTypeShort;

import java.util.stream.Collectors;

@Component
public class AreaMapper implements Transform<Area, moscow.ptnl.contingent.area.model.area.AreaInfo> {

    @Autowired
    private AreaTypeShortMapper areaTypeShortMapper;

    @Autowired
    private AreaMedicalEmployeeMapper areaMedicalEmployeeMapper;

    @Override
    public Area entityToDtoTransform(moscow.ptnl.contingent.area.model.area.AreaInfo entityObject) {
        Area area = new Area();

        moscow.ptnl.contingent.area.entity.area.Area areaObj = entityObject.getArea();
        area.setId(areaObj.getId());
        area.setMoId(areaObj.getMoId());
        area.setMuId(areaObj.getMuId());
        area.setNumber(areaObj.getNumber());
        area.setDescription(areaObj.getDescription());
        area.setAreaType(areaTypeShortMapper.entityToDtoTransform(areaObj.getAreaType()));
        area.setAgeMin(areaObj.getAgeMin());
        area.setAgeMax(areaObj.getAgeMax());
        area.setAgeMinM(areaObj.getAgeMMin());
        area.setAgeMaxM(areaObj.getAgeMMax());
        area.setAgeMinW(areaObj.getAgeWMin());
        area.setAgeMaxW(areaObj.getAgeWMax());
        area.setAutoAssignForAttachment(areaObj.getAutoAssignForAttach());
        area.setAttachByMedicalReason(areaObj.getAttachByMedicalReason());
        area.setArchive(Boolean.TRUE.equals(areaObj.getArchived()));

        if (!entityObject.getMainAreaMedicalEmployees().isEmpty()) {
            Area.MedicalEmployees medicalEmployees = new Area.MedicalEmployees();
            medicalEmployees.getMedicalEmployees().addAll(entityObject.getMainAreaMedicalEmployees().stream()
                    .map(areaMedicalEmployeeMapper::entityToDtoTransform)
                    .collect(Collectors.toList()));
            area.setMedicalEmployees(medicalEmployees);
        }

        if (!entityObject.getReplacementAreaMedicalEmployees().isEmpty()) {
            Area.MedicalEmployees medicalEmployees = new Area.MedicalEmployees();
            medicalEmployees.getMedicalEmployees().addAll(entityObject.getReplacementAreaMedicalEmployees().stream()
                    .map(areaMedicalEmployeeMapper::entityToDtoTransform)
                    .collect(Collectors.toList()));
            area.setMedicalEmployees(medicalEmployees);
        }

        if (!entityObject.getArea().getPrimaryAreaTypes().isEmpty()) {
            Area.PrimaryAreaTypeCodes areaTypeCodes = new Area.PrimaryAreaTypeCodes();

            entityObject.getArea().getPrimaryAreaTypes()
                .stream().map(AreaToAreaType::getAreaType)
                    .map(aat -> {
                        AreaTypeShort areaTypeShort = new AreaTypeShort();
                        areaTypeShort.setCode(aat.getCode());
                        areaTypeShort.setName(aat.getTitle());
                        return areaTypeShort;
                    })
                    .collect(Collectors.toList());
            area.setPrimaryAreaTypeCodes(areaTypeCodes);
        }
        return area;
    }

    @Override
    public moscow.ptnl.contingent.area.model.area.AreaInfo dtoToEntityTransform(Area dtoObject) {
        return null;
    }
}
