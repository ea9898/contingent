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
        areaAddress.setMoId(entityObject.getMoId());
        areaAddress.setMuId(entityObject.getMuId());
        areaAddress.setAreaId(entityObject.getAreaId());
        areaAddress.setAreaAddressId(entityObject.getAreaAddressId());
        areaAddress.setAddress(addressMapper.entityToDtoTransform(entityObject.getAddresses()));
        return areaAddress;
    }

    public moscow.ptnl.contingent.area.model.area.AddressArea entityToModelTransform(moscow.ptnl.contingent.area.entity.area.AreaAddress entityObject) {
        moscow.ptnl.contingent.area.model.area.AddressArea areaAddress = new moscow.ptnl.contingent.area.model.area.AddressArea();
        areaAddress.setMoId(entityObject.getArea().getMoId());
        areaAddress.setMuId(entityObject.getArea().getMuId());
        areaAddress.setAreaId(entityObject.getArea().getId());
        areaAddress.setAreaAddressId(entityObject.getId());
        areaAddress.setAddresses(entityObject.getAddress());
        return areaAddress;
    }

    @Override
    public moscow.ptnl.contingent.area.model.area.AddressArea dtoToEntityTransform(AreaAddress dtoObject) {
        return null;
    }
}
