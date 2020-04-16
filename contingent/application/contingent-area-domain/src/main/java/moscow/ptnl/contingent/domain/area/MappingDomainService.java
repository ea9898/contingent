package moscow.ptnl.contingent.domain.area;

import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;

public interface MappingDomainService {

    Addresses dtoToEntityTransform(AddressRegistry addressRegistry);
}
