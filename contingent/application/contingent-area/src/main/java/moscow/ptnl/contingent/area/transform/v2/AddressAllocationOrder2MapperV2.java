package moscow.ptnl.contingent.area.transform.v2;

import moscow.ptnl.contingent.domain.area.entity.AddressAllocationOrders;

import org.mapstruct.Mapper;
import org.mapstruct.Mappings;

import ru.mos.emias.contingent2.core.v2.Order;

@Mapper(componentModel="spring")
public abstract class AddressAllocationOrder2MapperV2 {

    @Mappings({
    })
    public abstract Order entityToDtoTransform(AddressAllocationOrders entity);
}
