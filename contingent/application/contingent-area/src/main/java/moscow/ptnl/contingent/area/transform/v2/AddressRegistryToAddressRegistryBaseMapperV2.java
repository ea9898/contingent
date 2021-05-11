package moscow.ptnl.contingent.area.transform.v2;

import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;
import org.mapstruct.Mapper;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;
import ru.mos.emias.contingent2.address.v2.AddressRegistryBaseType;

@Mapper(componentModel="spring")
public interface AddressRegistryToAddressRegistryBaseMapperV2 {

    AddressRegistryToAddressRegistryBaseMapperV2 MAPPER = Mappers.getMapper(AddressRegistryToAddressRegistryBaseMapperV2.class);

    @Mappings({})
    AddressRegistryBaseType entityToDtoTransform(AddressRegistry entityObject);

    @Mappings({})
    AddressRegistry dtoToEntityTransform(AddressRegistryBaseType dtoObject);


}
