package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.area.transform.model.options.GetAreaListBriefOptions.ShowMeValues;
import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;

import org.mapstruct.Context;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;

import ru.mos.emias.contingent2.core.AreaBrief;

import java.util.Comparator;
import java.util.stream.Collectors;

@Mapper(componentModel="spring")
public interface AreaBriefMapper {

    AreaBriefMapper MAPPER = Mappers.getMapper(AreaBriefMapper.class);

    @Mappings({
            @Mapping(source = "id", target = "id"),
            @Mapping(source = "moId", target = "moId"),
            @Mapping(source = "muId", target = "muId"),
            @Mapping(source = "number", target = "number"),
            @Mapping(source = "areaType.code", target = "areaTypeCode"),
            @Mapping(source = "areaType.title", target = "areaTypeTitle"),
            @Mapping(source = "archived", target = "archive"),
            @Mapping(target = "medicalEmployees", expression = "java( ShowMeValues.ALL.equals(employees) ? map(entity.getActualMedicalEmployees())"
                    + ": (ShowMeValues.NONE.equals(employees) ? null : map(entity.getActualMainMedicalEmployees())) )")
    })
    AreaBrief entityToDtoTransform(Area entity, @Context ShowMeValues employees);

    @Mappings({
            @Mapping(source = "medicalEmployeeJobId", target = "medicalEmployeeJobInfoId"),
            @Mapping(source = "replacement", target = "isReplacement"),
    })
    AreaBrief.MedicalEmployees.MedicalEmployee entityToDtoTransform(AreaMedicalEmployees entity);

    default AreaBrief.MedicalEmployees map(java.util.Set<AreaMedicalEmployees> value) {
        java.util.Set<AreaMedicalEmployees> actualMe = value.stream().filter(me -> me.getError() == null || !me.getError()).collect(Collectors.toSet());
        if (actualMe.isEmpty()) {
            return null;
        }
        AreaBrief.MedicalEmployees employees = new AreaBrief.MedicalEmployees();
        employees.getMedicalEmployees().addAll(actualMe.stream()
                .map(this::entityToDtoTransform)
                .sorted(Comparator.comparing(AreaBrief.MedicalEmployees.MedicalEmployee::isIsReplacement))
                .collect(Collectors.toList()));

        return employees;
    }
}
