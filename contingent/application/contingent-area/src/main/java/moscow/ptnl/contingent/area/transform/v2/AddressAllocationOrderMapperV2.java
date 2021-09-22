package moscow.ptnl.contingent.area.transform.v2;

import moscow.ptnl.contingent.domain.area.entity.AddressAllocationOrders;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;
import ru.mos.emias.contingent2.core.v2.AddressAllocationOrder;

@Mapper(componentModel="spring")
public interface AddressAllocationOrderMapperV2 {

    AddressAllocationOrderMapperV2 MAPPER = Mappers.getMapper(AddressAllocationOrderMapperV2.class);

    AddressAllocationOrder entityToDtoTransform(AddressAllocationOrders entityObject);

    AddressAllocationOrders dtoToEntityTransform(AddressAllocationOrder dtoObject);
}
