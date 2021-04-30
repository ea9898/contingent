package moscow.ptnl.contingent.area.transform.v2;

import moscow.ptnl.contingent.domain.area.entity.AreaMuService;
import moscow.ptnl.contingent.domain.area.entity.AreaToAreaType;
import moscow.ptnl.contingent.domain.area.model.area.AreaInfo;
import moscow.ptnl.contingent.transform.Transform;

import org.springframework.beans.factory.annotation.Autowired;

import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.v2.AreaTypeClass;
import ru.mos.emias.contingent2.core.v2.AreaTypeKind;
import ru.mos.emias.contingent2.core.v2.AreaTypeProfile;
import ru.mos.emias.contingent2.core.v2.AreaTypeShort;
import ru.mos.emias.contingent2.core.v2.MedicalEmployee;
import ru.mos.emias.contingent2.core.v2.Area;
import ru.mos.emias.contingent2.core.v2.MuService;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author sorlov
 */
@Component
public class AreaMapperV2 implements Transform<Area, AreaInfo> {

    @Autowired
    private AreaTypeShortMapperV2 areaTypeShortMapper;

    @Autowired
    private AreaMedicalEmployeeMapperV2 areaMedicalEmployeeMapper;

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

        if (areaObj.getAreaTypeProfile() != null) {
            area.setAreaTypeProfile(new CodeNameTypeMapper<>(new AreaTypeProfile(), areaObj.getAreaTypeProfile()).entityToDtoTransform());
        }
        if (entityObject.getAreaServicedMUs() != null && !entityObject.getAreaServicedMUs().isEmpty()) {
            MuService muService = new MuService();
            muService.getMuIds().addAll(entityObject.getAreaServicedMUs().stream().map(AreaMuService::getMuId).collect(Collectors.toList()));
            area.setMuService(muService);
        }
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
