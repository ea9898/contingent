package moscow.ptnl.contingent.area.transform;

import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

@Mapper(componentModel="spring")
public interface SearchAreaAddressCloner {

    SearchAreaAddressCloner MAPPER = Mappers.getMapper( SearchAreaAddressCloner.class );

    SearchAreaAddress clone(SearchAreaAddress customerDto);

    SearchAreaAddress dtoToModel(ru.mos.emias.contingent2.address.SearchAreaAddress address);

}
