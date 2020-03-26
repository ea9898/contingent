package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.transform.AddressMapper;
import moscow.ptnl.contingent.domain.area.MappingDomainService;
import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MappingDomainServiceImpl implements MappingDomainService {

    @Autowired
    private AddressMapper addressMapper;

    @Override
    public Addresses dtoToEntityTransform(AddressRegistry addressRegistry) {
        return addressMapper.dtoToEntityTransform(addressRegistry);
    }
}
