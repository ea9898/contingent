package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.security.UserContext;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;

@Mapper(componentModel="spring")
public abstract class UserContextMapper {

    @Mappings({
            @Mapping(target="userRights", source="userRights.userRightIds"),
            @Mapping(target="userRoleSystemWide", source="isUserRoleSystemWide")
    })
    public abstract UserContext dtoToEntityTransform(ru.mos.emias.system.v1.usercontext.UserContext userContext);

    @Mappings({
            @Mapping(target="userRights.userRightIds", source="userRights"),
            @Mapping(target="isUserRoleSystemWide", source="userRoleSystemWide")
    })
    public abstract ru.mos.emias.system.v1.usercontext.UserContext entityToDtoTransform(UserContext userContext);

}
