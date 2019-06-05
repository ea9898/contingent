package moscow.ptnl.contingent.area.transform;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.AreaAddress;

@Component
public class AreaAddressMapper implements Transform<AreaAddress, moscow.ptnl.contingent.area.entity.area.AreaAddress> {

    @Autowired
    private AddressMapper addressMapper;

    @Override
    public AreaAddress entityToDtoTransform(moscow.ptnl.contingent.area.entity.area.AreaAddress entityObject) {
        AreaAddress address = new AreaAddress();
        address.setAreaAddressId(entityObject.getId());
        if (entityObject.getAddress() != null) {
            address.setAddress(addressMapper.entityToDtoTransform(entityObject.getAddress()));
        }
        return address;
    }

    @Override
    public moscow.ptnl.contingent.area.entity.area.AreaAddress dtoToEntityTransform(AreaAddress dtoObject) {
        return null;
    }
}
