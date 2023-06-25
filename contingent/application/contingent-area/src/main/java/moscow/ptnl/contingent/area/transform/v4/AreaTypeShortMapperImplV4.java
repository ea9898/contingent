package moscow.ptnl.contingent.area.transform.v4;

import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import org.mapstruct.Mapper;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.v4.AreaTypeShort;

@Component
public class AreaTypeShortMapperImplV4 implements AreaTypeShortMapperV4 {

    @Override
    public AreaTypeShort entityToDtoTransform(AreaType entityObject) {
        if ( entityObject == null ) {
            return null;
        }

        AreaTypeShort areaTypeShort = new AreaTypeShort();

        areaTypeShort.setName( entityObject.getTitle() );
        if ( entityObject.getCode() != null ) {
            areaTypeShort.setCode( entityObject.getCode() );
        }

        return areaTypeShort;
    }

    @Override
    public AreaType dtoToEntityTransform(ru.mos.emias.contingent2.core.v4.AreaTypeShort dtoObject) {
        if ( dtoObject == null ) {
            return null;
        }

        AreaType areaType = new AreaType();

        areaType.setTitle( dtoObject.getName() );
        areaType.setCode( dtoObject.getCode() );

        return areaType;
    }
}
