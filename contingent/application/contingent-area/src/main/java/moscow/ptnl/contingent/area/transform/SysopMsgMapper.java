package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.area.entity.sysop.SysopMsg;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;
import ru.mos.emias.contingent2.sysop.types.ErrorMessage;
import ru.mos.emias.contingent2.sysop.types.ErrorMessageCollection;

import java.util.Set;
import java.util.stream.Collectors;

@Mapper(componentModel="spring", uses = { SysopMsgParamMapper.class })
public interface SysopMsgMapper {

    SysopMsgMapper MAPPER = Mappers.getMapper(SysopMsgMapper.class);

    @Mappings({
            @Mapping(source = "type", target = "type"),
            @Mapping(source = "code", target = "code"),
            @Mapping(source = "message", target = "message"),
            @Mapping(source = "params", target = "parameters"),
            @Mapping(source = "childMessages", target = "messages")
    })
    ErrorMessage entityToDtoTransform(SysopMsg entityObject);

    default ErrorMessageCollection map(Set<SysopMsg> value) {
        if (value == null || value.isEmpty()) {
            return null;
        }
        ErrorMessageCollection messageCollection = new ErrorMessageCollection();
        messageCollection.getMessages().addAll(value.stream().map(this::entityToDtoTransform).collect(Collectors.toSet()));

        return messageCollection;
    }
}
