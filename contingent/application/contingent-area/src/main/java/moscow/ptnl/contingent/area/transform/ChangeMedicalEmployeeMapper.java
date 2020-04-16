package moscow.ptnl.contingent.area.transform;

import org.mapstruct.Mapper;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;
import ru.mos.emias.contingent2.core.ChangeMedicalEmployee;

@Mapper(componentModel="spring")
public interface ChangeMedicalEmployeeMapper {

    ChangeMedicalEmployeeMapper MAPPER = Mappers.getMapper(ChangeMedicalEmployeeMapper.class);

    @Mappings({})
    ChangeMedicalEmployee entityToDtoTransform(moscow.ptnl.contingent.domain.area.model.area.ChangeMedicalEmployee entityObject);

    @Mappings({})
    moscow.ptnl.contingent.domain.area.model.area.ChangeMedicalEmployee dtoToEntityTransform(ChangeMedicalEmployee dtoObject);


}
