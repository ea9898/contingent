package moscow.ptnl.contingent.transform;

import moscow.ptnl.contingent.domain.area.entity.AreaToAreaType;
import moscow.ptnl.contingent.domain.area.model.area.AreaInfo;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.Area;
import ru.mos.emias.contingent2.core.AreaTypeClass;
import ru.mos.emias.contingent2.core.AreaTypeKind;
import ru.mos.emias.contingent2.core.AreaTypeShort;
import ru.mos.emias.contingent2.core.MedicalEmployee;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

@Component
public class AreaMapper implements Transform<Area, AreaInfo> {

    @Autowired
    private AreaTypeShortMapper areaTypeShortMapper;

    @Autowired
    private AreaMedicalEmployeeMapper areaMedicalEmployeeMapper;

    @Override
    public Area entityToDtoTransform(AreaInfo entityObject) {
        Area area = new Area();

        moscow.ptnl.contingent.domain.area.entity.Area areaObj = entityObject.getArea();
        area.setId(areaObj.getId());
        area.setMoId(areaObj.getMoId());
        area.setMuId(areaObj.getMuId());
        area.setNumber(areaObj.getNumber());
        area.setDescription(areaObj.getDescription());
        area.setAreaType(areaTypeShortMapper.entityToDtoTransform(areaObj.getAreaType()));
        area.setAreaTypeClass(new CodeNameTypeMapper<>(new AreaTypeClass(), areaObj.getAreaType().getAreaTypeClass()).entityToDtoTransform());
        area.setAreaTypeKind(new CodeNameTypeMapper<>(new AreaTypeKind(), areaObj.getAreaType().getAreaTypeKind()).entityToDtoTransform());
        area.setAgeMin(areaObj.getAgeMin());
        area.setAgeMax(areaObj.getAgeMax());
        area.setAgeMinM(areaObj.getAgeMMin());
        area.setAgeMaxM(areaObj.getAgeMMax());
        area.setAgeMinW(areaObj.getAgeWMin());
        area.setAgeMaxW(areaObj.getAgeWMax());
        area.setAutoAssignForAttachment(areaObj.getAutoAssignForAttach());
        area.setAttachByMedicalReason(areaObj.getAttachByMedicalReason());
        area.setArchive(Boolean.TRUE.equals(areaObj.getArchived()));

        List<MedicalEmployee> employees = new ArrayList<>();

        if (entityObject.getMainAreaMedicalEmployees() != null
                && !entityObject.getMainAreaMedicalEmployees().isEmpty()) {
            employees.addAll(entityObject.getMainAreaMedicalEmployees().stream()
                    .map(areaMedicalEmployeeMapper::entityToDtoTransform)
                    .collect(Collectors.toList()));
        }

        if (entityObject.getReplacementAreaMedicalEmployees() != null
                && !entityObject.getReplacementAreaMedicalEmployees().isEmpty()) {
            employees.addAll(entityObject.getReplacementAreaMedicalEmployees().stream()
                    .map(areaMedicalEmployeeMapper::entityToDtoTransform)
                    .collect(Collectors.toList()));

        }
        if (!employees.isEmpty()) {
            Area.MedicalEmployees medicalEmployees = new Area.MedicalEmployees();
            medicalEmployees.getMedicalEmployees().addAll(employees);
            area.setMedicalEmployees(medicalEmployees);
        }

        if (entityObject.getArea() != null && entityObject.getArea().getPrimaryAreaTypes() != null
                && !entityObject.getArea().getPrimaryAreaTypes().isEmpty()) {
            Area.PrimaryAreaTypeCodes areaTypeCodes = new Area.PrimaryAreaTypeCodes();

            areaTypeCodes.getAreaTypes().addAll(entityObject.getArea().getPrimaryAreaTypes()
                .stream().map(AreaToAreaType::getAreaType)
                    .map(aat -> {
                        AreaTypeShort areaTypeShort = new AreaTypeShort();
                        areaTypeShort.setCode(aat.getCode());
                        areaTypeShort.setName(aat.getTitle());
                        return areaTypeShort;
                    })
                    .collect(Collectors.toList()));
            area.setPrimaryAreaTypeCodes(areaTypeCodes);
        }
        return area;
    }

    @Override
    public AreaInfo dtoToEntityTransform(Area dtoObject) {
        return null;
    }
}
