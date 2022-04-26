package moscow.ptnl.contingent.area.transform.v3;

import moscow.ptnl.contingent.domain.area.model.area.AreaFullHistory;

import moscow.ptnl.contingent.domain.history.EntityConverterHelper;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;

import ru.mos.emias.contingent2.core.v3.AreaHistory;

import java.time.LocalDate;
import java.time.LocalDateTime;

@Mapper(componentModel="spring")
public interface GetAreaHistoryMapperV3 {

    GetAreaHistoryMapperV3 MAPPER = Mappers.getMapper(GetAreaHistoryMapperV3.class);

    @Mappings({
            @Mapping(target="eventType", expression="java( mapEventType(entity) )"),
            @Mapping(target="updateDate", source="updateDate"),
            @Mapping(target="userLogin", source="userLogin"),
            @Mapping(target="userJobId", expression="java( mapUserJobId(entity) )"),
            @Mapping(target="changeData.description.oldValue", source="descriptionOld"),
            @Mapping(target="changeData.description.newValue", source="descriptionNew"),
            @Mapping(target="changeData.number.oldValue", source="numberOld"),
            @Mapping(target="changeData.number.newValue", source="numberNew"),
            @Mapping(target="changeData.createDate.oldValue", source="createDateOld"),
            @Mapping(target="changeData.createDate.newValue", source="createDateNew"),
            @Mapping(target="changeData.archived.oldValue", source="archivedOld"),
            @Mapping(target="changeData.archived.newValue", source="archivedNew")
    })
    AreaHistory.AreaEvents.Event entityToDtoTransform(AreaFullHistory.AreaEvent entity);

    @Mappings({
            @Mapping(target="eventType", expression="java( mapEventType(entity) )"),
            @Mapping(target="updateDate", source="updateDate"),
            @Mapping(target="userLogin", source="userLogin"),
            @Mapping(target="medicalEmployeeJobId", source="medicalEmployeeJobId"),
            @Mapping(target="userJobId", expression="java( mapUserJobId(entity) )"),
            @Mapping(target="changeData.startDate.oldValue", source="startDateOld"),
            @Mapping(target="changeData.startDate.newValue", source="startDateNew"),
            @Mapping(target="changeData.endDate.oldValue", source="endDateOld"),
            @Mapping(target="changeData.endDate.newValue", source="endDateNew"),
            @Mapping(target="changeData.tempDutyStartDate.oldValue", source="tempDutyStartDateOld"),
            @Mapping(target="changeData.tempDutyStartDate.newValue", source="tempDutyStartDateNew"),
            @Mapping(target="changeData.isReplacement.oldValue", source="isReplacementOld"),
            @Mapping(target="changeData.isReplacement.newValue", source="isReplacementNew"),
            @Mapping(target="changeData.isError.oldValue", source="isErrorOld"),
            @Mapping(target="changeData.isError.newValue", source="isErrorNew")
    })
    AreaHistory.MedicalEmployeeEvents.Event entityToDtoTransform(AreaFullHistory.MedicalEmployeeEvent entity);

    default LocalDateTime mapDateTime(String value) {
        if (value == null || value.trim().contains(" ") || value.contains("T")) {
            return EntityConverterHelper.parseValue(value, LocalDateTime.class);
        }
        return EntityConverterHelper.parseValue(value, LocalDate.class).atStartOfDay();
    }

    default Boolean mapBoolean(String value) {
        return EntityConverterHelper.parseValue(value, Boolean.class);
    }

    default Long mapLong(String value) {
        return EntityConverterHelper.parseValue(value, Long.class);
    }

    default String mapEventType(AreaFullHistory.AreaEvent entity) {
        if (entity.getArchivedOld() == null) {
            return "create";
        }
        else if ("true".equalsIgnoreCase(entity.getArchivedNew())) {
            return "delete";
        }
        return "update";
    }

    default String mapEventType(AreaFullHistory.MedicalEmployeeEvent entity) {
        if (entity.getStartDateOld() == null) {
            return "create";
        }
        else if (entity.getEndDateNew() != null) {
            return "delete";
        }
        return "update";
    }

    default Long mapUserJobId(AreaFullHistory.Event entity) {
        if (entity.getUserJobId() == null || entity.getUserJobId().longValue() == 0 || entity.getUserJobId().longValue() == 1) {
            return 0L;
        }
        return entity.getUserJobId().longValue();
    }
}
