package moscow.ptnl.contingent.transform;

import org.mapstruct.Mapper;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;
import ru.mos.emias.contingent2.core.AddMedicalEmployee;

@Mapper(componentModel="spring")
public interface AddMedicalEmployeeMapper {

    AddMedicalEmployeeMapper MAPPER = Mappers.getMapper(AddMedicalEmployeeMapper.class);

    @Mappings({})
    AddMedicalEmployee entityToDtoTransform(moscow.ptnl.contingent.domain.area.model.area.AddMedicalEmployee entityObject);

    @Mappings({})
    moscow.ptnl.contingent.domain.area.model.area.AddMedicalEmployee dtoToEntityTransform(AddMedicalEmployee dtoObject);


}
