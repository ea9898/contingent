package moscow.ptnl.contingent.area.transform.v3;

import moscow.ptnl.contingent.domain.area.entity.AreaMuService;
import moscow.ptnl.contingent.domain.area.entity.AreaToAreaType;
import moscow.ptnl.contingent.domain.area.model.area.AreaInfo;
import moscow.ptnl.contingent.transform.Transform;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.v3.Area;
import ru.mos.emias.contingent2.core.v3.AreaSearchResult;
import ru.mos.emias.contingent2.core.v3.AreaTypeClass;
import ru.mos.emias.contingent2.core.v3.AreaTypeKind;
import ru.mos.emias.contingent2.core.v3.AreaTypeShort;
import ru.mos.emias.contingent2.core.v3.MedicalEmployee;
import ru.mos.emias.contingent2.core.v3.MuService;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author sorlov
 */
@Component
public class AreaMapperV3 implements Transform<Area, AreaInfo> {

    @Autowired
    private AreaTypeShortMapperV3 areaTypeShortMapper;

    @Autowired
    private AreaMedicalEmployeeMapperV3 areaMedicalEmployeeMapper;

    @Autowired
    private CodeNameTypeMapperV3 codeNameTypeMapper;

    public AreaSearchResult areaSearchResultTransform(AreaInfo entityObject) {
        AreaSearchResult result = entityToDtoTransform(entityObject, new AreaSearchResult());

        if (entityObject.getArea().getAreaType() != null) {
            result.setResidentsBindRate(entityObject.getArea().getAreaType().getResidentsBindRate());
        }
        return result;
    }

    @Override
    public Area entityToDtoTransform(AreaInfo entityObject) {
        return entityToDtoTransform(entityObject, new Area());
    }

    private  <T extends Area> T entityToDtoTransform(AreaInfo entityObject, T area) {
        moscow.ptnl.contingent.domain.area.entity.Area areaObj = entityObject.getArea();
        area.setId(areaObj.getId());
        area.setMoId(areaObj.getMoId());
        area.setMuId(areaObj.getMuId());
        area.setNumber(areaObj.getNumber());
        area.setDescription(areaObj.getDescription());
        area.setAreaType(areaTypeShortMapper.entityToDtoTransform(areaObj.getAreaType()));
        area.setAreaTypeClass(codeNameTypeMapper.entityToDtoTransform(areaObj.getAreaType().getAreaTypeClass(), AreaTypeClass.class));
        area.setAreaTypeKind(codeNameTypeMapper.entityToDtoTransform(areaObj.getAreaType().getAreaTypeKind(), AreaTypeKind.class));
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
