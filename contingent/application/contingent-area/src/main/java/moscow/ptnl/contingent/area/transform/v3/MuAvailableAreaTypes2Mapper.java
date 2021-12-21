package moscow.ptnl.contingent.area.transform.v3;

import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;
import ru.mos.emias.contingent2.area.v3.types.GetMuAvailableAreaTypes2Response;

@Mapper(componentModel="spring")
public interface MuAvailableAreaTypes2Mapper {

    MuAvailableAreaTypes2Mapper MAPPER = Mappers.getMapper(MuAvailableAreaTypes2Mapper.class);

    @Mappings({
            @Mapping(source = "title", target = "name"),
    })
    GetMuAvailableAreaTypes2Response.MuAvailableAreaType entityToDtoTransform(AreaType entityObject);
}
