package moscow.ptnl.contingent.area.transform.v3;

import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;
import moscow.ptnl.contingent.domain.area.entity.AreaMuService;
import moscow.ptnl.contingent.domain.area.model.area.AreaInfo;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.springframework.beans.factory.annotation.Autowired;
import ru.mos.emias.contingent2.core.v3.AreaBrief;
import ru.mos.emias.contingent2.core.v3.AreaTypeProfile;
import ru.mos.emias.contingent2.core.v3.MuService;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

@Mapper(componentModel="spring", imports = { AreaTypeProfile.class })
public abstract class AreaBriefMapperV3 {

    @Autowired
    CodeNameTypeMapperV3 codeNameTypeMapper;

    @Mappings({
            @Mapping(source = "area.id", target = "id"),
            @Mapping(source = "area.moId", target = "moId"),
            @Mapping(source = "area.muId", target = "muId"),
            @Mapping(source = "area.number", target = "number"),
            @Mapping(source = "area.areaType.code", target = "areaTypeCode"),
            @Mapping(source = "area.areaType.title", target = "areaTypeTitle"),
            @Mapping(source = "area.archived", target = "archive"),
            @Mapping(target = "medicalEmployees", expression = "java( map(entity.getMainAreaMedicalEmployees()) )"),
            @Mapping(source = "areaServicedMUs", target = "muService"),
            @Mapping(target = "dateCreated", expression = "java( entity.getArea().getCreateDate().toLocalDate() )")
    })
    public abstract AreaBrief entityToDtoTransform(AreaInfo entity);

    @Mappings({
            @Mapping(source = "medicalEmployeeJobId", target = "medicalEmployeeJobInfoId"),
            @Mapping(source = "tempDutyStartDate", target = "tempDutyStartDate"),
            @Mapping(source = "replacement", target = "isReplacement")
    })
    public abstract AreaBrief.MedicalEmployees.MedicalEmployee entityToDtoTransform(AreaMedicalEmployees entity);

    public MuService entityToDtoTransform(List<AreaMuService> entities) {
        if (entities == null || entities.isEmpty()) {
            return null;
        }
        MuService muService = new MuService();
        muService.getMuIds().addAll(entities.stream().map(AreaMuService::getMuId).collect(Collectors.toList()));

        return muService;
    }

    public AreaBrief.MedicalEmployees map(List<AreaMedicalEmployees> value) {
        java.util.Set<AreaMedicalEmployees> actualMe = value.stream().filter(me -> me.getError() == null || !me.getError()).collect(Collectors.toSet());
        if (actualMe.isEmpty()) {
            return null;
        }
        AreaBrief.MedicalEmployees employees = new AreaBrief.MedicalEmployees();
        employees.getMedicalEmployees().addAll(actualMe.stream()
                .map(this::entityToDtoTransform)
                .sorted(Comparator.comparing(AreaBrief.MedicalEmployees.MedicalEmployee::isIsReplacement)
                        .thenComparing(e -> (e.getTempDutyStartDate() == null && e.isIsReplacement())
                                || (e.getTempDutyStartDate() != null && !e.isIsReplacement())))
                .collect(Collectors.toList()));

        return employees;
    }
}
