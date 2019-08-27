package moscow.ptnl.contingent.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.NsiAddressFormingElement;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface AddressFormingElementRepository {

	List<NsiAddressFormingElement> getAddressFormingElements(long globalId, int level);

	List<NsiAddressFormingElement> findAfeByIdAndLevel(long afeId, int level);

	NsiAddressFormingElement findAfeByGlobalId(Long globalId);

}
