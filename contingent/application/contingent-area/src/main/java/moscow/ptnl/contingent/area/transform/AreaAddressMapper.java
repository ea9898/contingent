package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.nsi.domain.area.NsiBuildingRegistry;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.Address;
import ru.mos.emias.contingent2.core.AreaAddress;

@Component
public class AreaAddressMapper implements Transform<AreaAddress, moscow.ptnl.contingent.area.model.area.AddressArea> {

    @Autowired
    private AddressMapper addressMapper;

    @Override
    public AreaAddress entityToDtoTransform(moscow.ptnl.contingent.area.model.area.AddressArea entityObject) {
        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setAreaAddressId(entityObject.getAreaAddressId());
        areaAddress.setAddress(addressMapper.entityToDtoTransform(entityObject.getAddresses()));
        return areaAddress;
    }

    @Override
    public moscow.ptnl.contingent.area.model.area.AddressArea dtoToEntityTransform(AreaAddress dtoObject) {
        return null;
    }
}
