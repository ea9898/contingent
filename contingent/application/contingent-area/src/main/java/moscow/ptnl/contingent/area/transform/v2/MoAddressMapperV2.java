package moscow.ptnl.contingent.area.transform.v2;

import moscow.ptnl.contingent.transform.Transform;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.v2.MoAddress;

@Component
public class MoAddressMapperV2 implements Transform<MoAddress, moscow.ptnl.contingent.domain.area.entity.MoAddress> {

    @Autowired
    private AddressBaseTypeMapperV2 addressBaseTypeMapper;

    @Override
    public MoAddress entityToDtoTransform(moscow.ptnl.contingent.domain.area.entity.MoAddress entityObject) {
        MoAddress address = new MoAddress();
        address.setMoAddressId(entityObject.getId());

        if (entityObject.getAreaType() != null) {
            address.setAreaTypeCode(entityObject.getAreaType().getCode());
        }
        if (entityObject.getAddressAllocationOrder() != null) {
            address.setOrderId(entityObject.getAddressAllocationOrder().getId());
        }
        if (entityObject.getAddress() != null) {
            address.setAddress(addressBaseTypeMapper.entityToDtoTransform(entityObject.getAddress()));
        }
        return address;
    }

    @Override
    public moscow.ptnl.contingent.domain.area.entity.MoAddress dtoToEntityTransform(MoAddress dtoObject) {
        return null;
    }
}
