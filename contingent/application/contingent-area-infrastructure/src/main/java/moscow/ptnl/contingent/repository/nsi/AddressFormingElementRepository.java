package moscow.ptnl.contingent.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.AddressFormingElement;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface AddressFormingElementRepository {

	List<AddressFormingElement> getAddressFormingElements(long globalId, int level);

	List<AddressFormingElement> findAfeByIdAndLevel(long afeId, int level);

	AddressFormingElement findAfeByGlobalId(Long globalId);

}
