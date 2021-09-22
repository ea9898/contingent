package moscow.ptnl.contingent.area.transform.v2;

import moscow.ptnl.contingent.transform.Transform;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.v2.AreaAddress;

@Component
public class AreaAddressMapperV2 implements Transform<AreaAddress, moscow.ptnl.contingent.domain.area.entity.AreaAddress> {

    @Autowired
    private AddressBaseTypeMapperV2 addressBaseTypeMapper;

    @Override
    public AreaAddress entityToDtoTransform(moscow.ptnl.contingent.domain.area.entity.AreaAddress entityObject) {
        AreaAddress areaAddress = new AreaAddress();
        areaAddress.setMoId(entityObject.getArea().getMoId());
        areaAddress.setMuId(entityObject.getArea().getMuId());
        areaAddress.setAreaId(entityObject.getArea().getId());
        areaAddress.setAreaAddressId(entityObject.getId());
        areaAddress.setAddress(addressBaseTypeMapper.entityToDtoTransform(entityObject.getAddress()));
        return areaAddress;
    }

    @Override
    public moscow.ptnl.contingent.domain.area.entity.AreaAddress dtoToEntityTransform(AreaAddress dtoObject) {
        return null;
    }
}
