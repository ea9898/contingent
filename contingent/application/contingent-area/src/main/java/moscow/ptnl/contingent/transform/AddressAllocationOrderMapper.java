package moscow.ptnl.contingent.transform;

import moscow.ptnl.contingent.domain.area.entity.AddressAllocationOrders;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;
import ru.mos.emias.contingent2.core.AddressAllocationOrder;

@Mapper(componentModel="spring")
public interface AddressAllocationOrderMapper {

    AddressAllocationOrderMapper MAPPER = Mappers.getMapper(AddressAllocationOrderMapper.class);

    AddressAllocationOrder entityToDtoTransform(AddressAllocationOrders entityObject);

    AddressAllocationOrders dtoToEntityTransform(AddressAllocationOrder dtoObject);
}
