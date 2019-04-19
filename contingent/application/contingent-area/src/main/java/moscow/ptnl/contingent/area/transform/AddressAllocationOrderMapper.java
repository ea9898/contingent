package moscow.ptnl.contingent.area.transform;

import org.springframework.stereotype.Component;
import ru.gov.emias2.contingent.v1.area.types.AddressAllocationOrder;

@Component
public class AddressAllocationOrderMapper implements Transform<AddressAllocationOrder, moscow.ptnl.contingent.area.entity.area.AddressAllocationOrder> {

    @Override
    public AddressAllocationOrder entityToDtoTransform(moscow.ptnl.contingent.area.entity.area.AddressAllocationOrder entityObject) {
        AddressAllocationOrder allocationOrder = new AddressAllocationOrder();
        allocationOrder.setId(entityObject.getId());
        allocationOrder.setNumber(entityObject.getNumber());
        allocationOrder.setDate(entityObject.getDate());
        allocationOrder.setName(entityObject.getName());
        allocationOrder.setOuz(entityObject.getOuz());

        return allocationOrder;
    }

    @Override
    public moscow.ptnl.contingent.area.entity.area.AddressAllocationOrder dtoToEntityTransform(AddressAllocationOrder dtoObject) {
        return null;
    }
}
