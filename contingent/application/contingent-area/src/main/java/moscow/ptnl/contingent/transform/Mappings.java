package moscow.ptnl.contingent.transform;

import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;

public interface Mappings {

    Addresses dtoToEntityTransform(AddressRegistry addressRegistry);

}
