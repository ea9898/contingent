package moscow.ptnl.contingent.nsi.domain.repository;

import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;
import moscow.ptnl.contingent.nsi.domain.area.NsiAddressFormingElement;

@NoRepositoryBean
public interface AddressFormingElementRepository {

	List<NsiAddressFormingElement> getAddressFormingElements(long globalId, int level);

	NsiAddressFormingElement findAfeByGlobalId(Long globalId);

}
