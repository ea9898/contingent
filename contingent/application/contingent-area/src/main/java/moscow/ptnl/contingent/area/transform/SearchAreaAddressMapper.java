package moscow.ptnl.contingent.area.transform;

import org.mapstruct.Mapper;
import org.mapstruct.Mappings;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.address.SearchAreaAddress;

@Mapper(componentModel="spring")
public interface SearchAreaAddressMapper {

    @Mappings({})
    SearchAreaAddress entityToDtoTransform(moscow.ptnl.contingent.domain.area.model.area.SearchAreaAddress entityObject);

    @Mappings({})
    moscow.ptnl.contingent.domain.area.model.area.SearchAreaAddress dtoToEntityTransform(SearchAreaAddress dtoObject);

}
