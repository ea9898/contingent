package moscow.ptnl.contingent.area.transform.v3;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;
import ru.mos.emias.contingent2.core.v3.AddMedicalEmployee;

@Mapper(componentModel="spring")
public interface AddMedicalEmployeeMapperV3 {

    AddMedicalEmployeeMapperV3 MAPPER = Mappers.getMapper(AddMedicalEmployeeMapperV3.class);

    @Mappings({
            @Mapping(source = "medicalEmployeeJobInfoId", target = "medicalEmployeeJobInfoId"),
            @Mapping(source = "snils", target = "snils"),
            @Mapping(source = "positionCode", target = "positionCode"),
            @Mapping(source = "subdivisionId", target = "subdivisionId"),
            @Mapping(source = "replacement", target = "isReplacement"),
            @Mapping(source = "tempDuty", target = "isTempDuty"),
            @Mapping(source = "startDate", target = "startDate"),
            @Mapping(source = "endDate", target = "endDate")
    })
    AddMedicalEmployee entityToDtoTransform(moscow.ptnl.contingent.domain.area.model.area.AddMedicalEmployee entityObject);

    @Mappings({
            @Mapping(source = "medicalEmployeeJobInfoId", target = "medicalEmployeeJobInfoId"),
            @Mapping(source = "snils", target = "snils"),
            @Mapping(source = "positionCode", target = "positionCode"),
            @Mapping(source = "subdivisionId", target = "subdivisionId"),
            @Mapping(source = "isReplacement", target = "replacement"),
            @Mapping(source = "isTempDuty", target = "tempDuty"),
            @Mapping(source = "startDate", target = "startDate"),
            @Mapping(source = "endDate", target = "endDate")
    })
    moscow.ptnl.contingent.domain.area.model.area.AddMedicalEmployee dtoToEntityTransform(AddMedicalEmployee dtoObject);


}
