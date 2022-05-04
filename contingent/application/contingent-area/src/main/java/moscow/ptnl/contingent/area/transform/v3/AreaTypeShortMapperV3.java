package moscow.ptnl.contingent.area.transform.v3;

import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;
import ru.mos.emias.contingent2.core.v3.AreaTypeShort;

@Mapper(componentModel="spring")
public interface AreaTypeShortMapperV3 {

    AreaTypeShortMapperV3 MAPPER = Mappers.getMapper(AreaTypeShortMapperV3.class);

    @Mappings({
            @Mapping(source = "title", target = "name"),
    })
    AreaTypeShort entityToDtoTransform(AreaType entityObject);

    @Mappings({
            @Mapping(source = "name", target = "title"),
    })
    AreaType dtoToEntityTransform(AreaTypeShort dtoObject);
}
