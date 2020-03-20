package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.area.entity.area.AddressAllocationOrders;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.AddressAllocationOrder;

@Mapper(componentModel="spring")
public interface AddressAllocationOrderMapper {

    AddressAllocationOrderMapper MAPPER = Mappers.getMapper(AddressAllocationOrderMapper.class);

    AddressAllocationOrder entityToDtoTransform(AddressAllocationOrders entityObject);

    AddressAllocationOrders dtoToEntityTransform(AddressAllocationOrder dtoObject);
}
