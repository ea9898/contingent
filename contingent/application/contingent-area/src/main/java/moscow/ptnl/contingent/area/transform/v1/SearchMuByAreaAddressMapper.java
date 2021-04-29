package moscow.ptnl.contingent.area.transform.v1;

import moscow.ptnl.contingent.domain.area.entity.Area;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;
import ru.mos.emias.contingent2.area.types.SearchMuByAreaAddressResponse;

@Mapper(componentModel="spring")
public interface SearchMuByAreaAddressMapper {

    SearchMuByAreaAddressMapper MAPPER = Mappers.getMapper(SearchMuByAreaAddressMapper.class);

    @Mappings({
            @Mapping(source = "muId", target = "muId"),
            @Mapping(source = "moId", target = "moId")
    })
    SearchMuByAreaAddressResponse.Result entityToDtoTransform(Area area);
}
