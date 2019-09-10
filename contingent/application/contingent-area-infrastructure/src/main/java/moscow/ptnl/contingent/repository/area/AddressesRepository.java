package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.Addresses;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;
import moscow.ptnl.contingent.nsi.domain.area.NsiAddressFormingElement;
import moscow.ptnl.contingent.nsi.domain.area.NsiBuildingRegistry;

@NoRepositoryBean
public interface AddressesRepository {

	List<Addresses> findAddresses(long level, NsiBuildingRegistry buildingRegistry, NsiAddressFormingElement addressFormingElement);
	List<Addresses> findAddresses(List<Long> areaAddressId, List<Long> nsiGlobalId);
}
