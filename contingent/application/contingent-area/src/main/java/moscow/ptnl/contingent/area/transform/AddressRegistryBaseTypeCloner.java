package moscow.ptnl.contingent.area.transform;

import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;
import ru.mos.emias.contingent2.address.AddressRegistryBaseType;

import java.util.List;
import java.util.Map;

@Mapper
public interface AddressRegistryBaseTypeCloner {

    AddressRegistryBaseTypeCloner MAPPER = Mappers.getMapper( AddressRegistryBaseTypeCloner.class );

    AddressRegistryBaseType clone(AddressRegistryBaseType customerDto);
}
