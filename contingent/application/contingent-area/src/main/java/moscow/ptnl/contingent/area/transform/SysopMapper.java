package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.area.entity.sysop.Sysop;
import org.mapstruct.Named;
import ru.mos.emias.contingent2.sysop.types.OperationCompletenessPercentage;
import ru.mos.emias.contingent2.sysop.types.OperationExecutionStatus;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;

@Mapper(componentModel="spring", uses = { SysopMsgMapper.class })
public interface SysopMapper {

    SysopMapper MAPPER = Mappers.getMapper(SysopMapper.class);

    @Mappings({
            @Mapping(source = "progress", target = "completenessProgress", qualifiedByName = "ToOperationCompletenessPercentage"),
            @Mapping(source = "completed", target = "isCompleted"),
            @Mapping(source = "successful", target = "hasSucceded"),
            @Mapping(source = "rootMessages", target = "messages")
    })
    OperationExecutionStatus entityToDtoTransform(Sysop entityObject);

    @Mappings({
            @Mapping(target = "completenessStatus", expression = "java(value == null ? null : value.shortValue())")
    })
    @Named("ToOperationCompletenessPercentage ")
    OperationCompletenessPercentage map(Integer value);
}
