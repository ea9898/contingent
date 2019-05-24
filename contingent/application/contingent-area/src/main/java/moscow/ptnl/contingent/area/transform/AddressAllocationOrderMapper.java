package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.area.entity.area.AddressAllocationOrders;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.AddressAllocationOrder;

@Component
public class AddressAllocationOrderMapper implements Transform<AddressAllocationOrder, AddressAllocationOrders> {

    @Override
    public AddressAllocationOrder entityToDtoTransform(AddressAllocationOrders entityObject) {
        AddressAllocationOrder allocationOrder = new AddressAllocationOrder();
        allocationOrder.setId(entityObject.getId());
        allocationOrder.setNumber(entityObject.getNumber());
        allocationOrder.setDate(entityObject.getDate());
        allocationOrder.setName(entityObject.getName());
//        allocationOrder.setOuz(entityObject.getOuz());

        return allocationOrder;
    }

    @Override
    public AddressAllocationOrders dtoToEntityTransform(AddressAllocationOrder dtoObject) {
        return null;
    }
}
