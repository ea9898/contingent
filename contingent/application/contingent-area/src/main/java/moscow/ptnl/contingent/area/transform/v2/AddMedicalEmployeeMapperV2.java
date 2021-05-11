package moscow.ptnl.contingent.area.transform.v2;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;
import ru.mos.emias.contingent2.core.v2.AddMedicalEmployee;

@Mapper(componentModel="spring")
public interface AddMedicalEmployeeMapperV2 {

    AddMedicalEmployeeMapperV2 MAPPER = Mappers.getMapper(AddMedicalEmployeeMapperV2.class);

    @Mappings({
            @Mapping(source = "medicalEmployeeJobInfoId", target = "medicalEmployeeJobInfoId"),
            @Mapping(source = "snils", target = "snils"),
            @Mapping(source = "positionCode", target = "positionCode"),
            @Mapping(source = "subdivisionId", target = "subdivisionId"),
            @Mapping(source = "replacement", target = "isReplacement"),
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
            @Mapping(source = "startDate", target = "startDate"),
            @Mapping(source = "endDate", target = "endDate")
    })
    moscow.ptnl.contingent.domain.area.model.area.AddMedicalEmployee dtoToEntityTransform(AddMedicalEmployee dtoObject);


}
