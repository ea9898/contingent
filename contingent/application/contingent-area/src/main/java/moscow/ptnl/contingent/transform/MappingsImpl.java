package moscow.ptnl.contingent.transform;

import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MappingsImpl implements Mappings {

    @Autowired
    private AddressMapper addressMapper;

    @Override
    public Addresses dtoToEntityTransform(AddressRegistry addressRegistry) {
        return addressMapper.dtoToEntityTransform(addressRegistry);
    }
}
