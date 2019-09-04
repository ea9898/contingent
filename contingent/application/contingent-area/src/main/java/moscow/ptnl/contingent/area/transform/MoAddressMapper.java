package moscow.ptnl.contingent.area.transform;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.MoAddress;

@Component
public class MoAddressMapper implements Transform<MoAddress, moscow.ptnl.contingent.area.entity.area.MoAddress> {

    @Autowired
    private AddressMapper addressMapper;

    @Override
    public MoAddress entityToDtoTransform(moscow.ptnl.contingent.area.entity.area.MoAddress entityObject) {
        throw new RuntimeException("переделать");
/*

        MoAddress address = new MoAddress();
        address.setMoAddressId(entityObject.getId());

        if (entityObject.getAreaType() != null) {
            address.setAreaTypeCode(entityObject.getAreaType().getCode());
        }
        if (entityObject.getAddressAllocationOrder() != null) {
            address.setOrderId(entityObject.getAddressAllocationOrder().getId());
        }
        if (entityObject.getAddress() != null) {
            address.setAddress(addressMapper.entityToDtoTransform(entityObject.getAddress()));
        }
        return address;
*/
    }

    @Override
    public moscow.ptnl.contingent.area.entity.area.MoAddress dtoToEntityTransform(MoAddress dtoObject) {
        return null;
    }
}
