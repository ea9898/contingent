package moscow.ptnl.contingent.area.transform.v3;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;
import ru.mos.emias.contingent2.core.v3.ChangeMedicalEmployee;

@Mapper(componentModel="spring")
public interface ChangeMedicalEmployeeMapperV3 {

    ChangeMedicalEmployeeMapperV3 MAPPER = Mappers.getMapper(ChangeMedicalEmployeeMapperV3.class);

    @Mappings({
            @Mapping(source = "tempDuty", target = "isTempDuty")
    })
    ChangeMedicalEmployee entityToDtoTransform(moscow.ptnl.contingent.domain.area.model.area.ChangeMedicalEmployee entityObject);

    @Mappings({
            @Mapping(source = "isTempDuty", target = "tempDuty")
    })
    moscow.ptnl.contingent.domain.area.model.area.ChangeMedicalEmployee dtoToEntityTransform(ChangeMedicalEmployee dtoObject);


}
