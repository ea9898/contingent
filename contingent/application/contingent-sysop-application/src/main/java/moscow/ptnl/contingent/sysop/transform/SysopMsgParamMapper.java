package moscow.ptnl.contingent.sysop.transform;

import moscow.ptnl.contingent.sysop.entity.SysopMsgParam;
import org.mapstruct.InheritInverseConfiguration;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;
import ru.mos.emias.contingent2.sysop.types.ErrorMessage;
import ru.mos.emias.contingent2.sysop.types.KeyValuePair;

import java.util.Set;
import java.util.stream.Collectors;

@Mapper(componentModel="spring")
public interface SysopMsgParamMapper {

    SysopMsgParamMapper MAPPER = Mappers.getMapper(SysopMsgParamMapper.class);

    @Mappings({
            @Mapping(source = "key", target = "key"),
            @Mapping(source = "value", target = "value")
    })
    KeyValuePair entityToDtoTransform(SysopMsgParam entityObject);

    @InheritInverseConfiguration
    SysopMsgParam dtoToEntityTransform(KeyValuePair dtoObject);

    default ErrorMessage.Parameters map(Set<SysopMsgParam> value) {
        if (value == null || value.isEmpty()) {
            return null;
        }
        ErrorMessage.Parameters messageCollection = new ErrorMessage.Parameters();
        messageCollection.getParameters().addAll(value.stream().map(this::entityToDtoTransform).collect(Collectors.toSet()));

        return messageCollection;
    }
}
