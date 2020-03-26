package moscow.ptnl.contingent.transform;

import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;
import org.mapstruct.Mapper;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;
import ru.mos.emias.contingent2.address.AddressRegistryBaseType;

@Mapper(componentModel="spring")
public interface AddressRegistryToAddressRegistryBaseMapper {

    AddressRegistryToAddressRegistryBaseMapper MAPPER = Mappers.getMapper(AddressRegistryToAddressRegistryBaseMapper.class);

    @Mappings({})
    AddressRegistryBaseType entityToDtoTransform(AddressRegistry entityObject);

    @Mappings({})
    AddressRegistry dtoToEntityTransform(AddressRegistryBaseType dtoObject);


}
