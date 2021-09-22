package moscow.ptnl.contingent.area.transform.v2;

import moscow.ptnl.contingent.domain.area.entity.Area;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;
import ru.mos.emias.contingent2.area.v2.types.SearchMuByAreaAddressResponse;

@Mapper(componentModel="spring")
public interface SearchMuByAreaAddressMapperV2 {

    SearchMuByAreaAddressMapperV2 MAPPER = Mappers.getMapper(SearchMuByAreaAddressMapperV2.class);

    @Mappings({
            @Mapping(source = "muId", target = "muId"),
            @Mapping(source = "moId", target = "moId")
    })
    SearchMuByAreaAddressResponse.Result.MuInfo entityToDtoTransform(Area area);
}
