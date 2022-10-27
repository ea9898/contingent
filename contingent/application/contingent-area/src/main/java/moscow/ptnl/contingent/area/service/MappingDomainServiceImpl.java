package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.transform.v1.AddressMapper;
import moscow.ptnl.contingent.domain.area.MappingDomainService;
import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

// TODO перенести трансформацию в domain area модуль
@Component
public class MappingDomainServiceImpl implements MappingDomainService {

    @Autowired
    private AddressMapper addressMapper;

    @Override
    public Addresses dtoToEntityTransform(AddressRegistry addressRegistry) {
        return addressMapper.dtoToEntityTransform(addressRegistry);
    }
}
