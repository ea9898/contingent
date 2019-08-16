package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.entity.nsi.NsiAddressFormingElement;
import moscow.ptnl.contingent.area.entity.nsi.NsiBuildingRegistry;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface AddressesRepository {

	List<Addresses> findAddresses(long level, NsiBuildingRegistry buildingRegistry, NsiAddressFormingElement addressFormingElement);
}
