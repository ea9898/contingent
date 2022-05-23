package moscow.ptnl.contingent.area.transform.v3;

import moscow.ptnl.contingent.domain.area.model.area.AreaOrEmployeeEvent;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;

import ru.mos.emias.contingent2.core.v3.HistoryEvent;
import ru.mos.emias.contingent2.core.v3.HistoryEvent.ChangeData.AttributeValues;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Mapper(componentModel="spring")
public interface GetAreaHistoryMapperV3 {

    GetAreaHistoryMapperV3 MAPPER = Mappers.getMapper(GetAreaHistoryMapperV3.class);

    @Mappings({
            @Mapping(target="eventType", expression="java( mapEventType(entity) )"),
            @Mapping(target="updateDate", source="updateDate"),
            @Mapping(target="userLogin", source="userLogin"),
            @Mapping(target="userJobId", expression="java( mapUserJobId(entity) )"),
            @Mapping(target="objectType", expression="java( mapObjectType(entity) )"),
            @Mapping(target="objectId", expression="java( mapObjectId(entity) )"),
            @Mapping(target="changeData", expression="java( mapChangeData(entity) )")
    })
    HistoryEvent entityToDtoTransform(AreaOrEmployeeEvent entity);

    default HistoryEvent.ChangeData mapChangeData(AreaOrEmployeeEvent entity) {
        List<AttributeValues> values = new ArrayList<>();

        if (entity.getObjType().intValue() == 2) {
            addAttributeValue("description", entity.getDescriptionOld(), entity.getDescriptionNew(), values);
            addAttributeValue("number", entity.getNumberOld(), entity.getNumberNew(), values);
            addAttributeValue("createDate", entity.getCreateDateOld(), entity.getCreateDateNew(), values);
            addAttributeValue("archived", entity.getArchivedOld(), entity.getArchivedNew(), values);
        }
        else if (entity.getObjType().intValue() == 1) {
            addAttributeValue("startDate", entity.getStartDateOld(), entity.getStartDateNew(), values);
            addAttributeValue("endDate", entity.getEndDateOld(), entity.getEndDateNew(), values);
            addAttributeValue("isError", entity.getIsErrorOld(), entity.getIsErrorNew(), values);
            addAttributeValue("tempDutyStartDate", entity.getTempDutyStartDateOld(), entity.getTempDutyStartDateNew(), values);
            addAttributeValue("isReplacement", entity.getIsReplacementOld(), entity.getIsReplacementNew(), values);
        }
        HistoryEvent.ChangeData changeData = new HistoryEvent.ChangeData();
        changeData.getAttributeValues().addAll(values);
        return changeData;
    }

    default void addAttributeValue(String attributeName, String oldValue, String newValue, List<AttributeValues> values) {
        if (!Objects.equals(oldValue, newValue)) {
            AttributeValues value = new AttributeValues();
            value.setAttributeName(attributeName);
            value.setOldValue(oldValue);
            value.setNewValue(newValue);
            values.add(value);
        }
    }

    default String mapEventType(AreaOrEmployeeEvent entity) {
        if (entity. getObjType().intValue() == 2) {
            if (entity.getCreateDateNew() != null && entity.getCreateDateOld() == null) {
                return "create";
            }
            else if ("true".equalsIgnoreCase(entity.getArchivedNew())) {
                return "delete";
            }
            return "update";
        }
        else if (entity.getObjType().intValue() == 1) {
            if (entity.getCreateDateNew() != null && entity.getStartDateOld() == null) {
                return "create";
            }
            else if (entity.getEndDateNew() != null) {
                return "delete";
            }
            return "update";
        }
        return null;
    }

    default Long mapObjectId(AreaOrEmployeeEvent entity) {
        if (entity.getObjType().intValue() == 2) {
            return entity.getObjectId().longValue();
        }
        else if (entity.getObjType().intValue() == 1) {
            return entity.getMedicalEmployeeJobId().longValue();
        }
        return null;
    }

    default String mapObjectType(AreaOrEmployeeEvent entity) {
        if (entity.getObjType().intValue() == 2) {
            return "AREA";
        }
        else if (entity.getObjType().intValue() == 1) {
            return "AREA_ME_JOB_ID";
        }
        return null;
    }

    default Long mapUserJobId(AreaOrEmployeeEvent entity) {
        if (entity.getUserJobId() == null || entity.getUserJobId().longValue() == 0 || entity.getUserJobId().longValue() == 1) {
            return 0L;
        }
        return entity.getUserJobId().longValue();
    }
}
