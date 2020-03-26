package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

@Mapper(componentModel="spring")
public interface AreaAddressClone {

    AreaAddressClone MAPPER = Mappers.getMapper( AreaAddressClone.class );

    AreaAddress clone(AreaAddress customerDto);

}
