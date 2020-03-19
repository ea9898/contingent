package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.domain.area.model.area.AddressArea;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.AreaAddress;

@Component
public class AreaAddressMapper implements Transform<AreaAddress, AddressArea> {

    @Autowired
    private AddressMapper addressMapper;

    @Override
    public AreaAddress entityToDtoTransform(AddressArea entityObject) {
        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setMoId(entityObject.getMoId());
        areaAddress.setMuId(entityObject.getMuId());
        areaAddress.setAreaId(entityObject.getAreaId());
        areaAddress.setAreaAddressId(entityObject.getAreaAddressId());
        areaAddress.setAddress(addressMapper.entityToDtoTransform(entityObject.getAddresses()));
        return areaAddress;
    }

    public AddressArea entityToModelTransform(moscow.ptnl.contingent.domain.area.entity.area.AreaAddress entityObject) {
        AddressArea areaAddress = new AddressArea();
        areaAddress.setMoId(entityObject.getArea().getMoId());
        areaAddress.setMuId(entityObject.getArea().getMuId());
        areaAddress.setAreaId(entityObject.getArea().getId());
        areaAddress.setAreaAddressId(entityObject.getId());
        areaAddress.setAddresses(entityObject.getAddress());
        return areaAddress;
    }

    @Override
    public AddressArea dtoToEntityTransform(AreaAddress dtoObject) {
        return null;
    }
}
