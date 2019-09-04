package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.AreaTypeShort;

@Mapper(componentModel="spring")
public interface AreaTypeShortMapper {

    AreaTypeShortMapper MAPPER = Mappers.getMapper(AreaTypeShortMapper.class);

    AreaTypeShort entityToDtoTransform(AreaType entityObject);

    AreaType dtoToEntityTransform(AreaTypeShort dtoObject);
}
