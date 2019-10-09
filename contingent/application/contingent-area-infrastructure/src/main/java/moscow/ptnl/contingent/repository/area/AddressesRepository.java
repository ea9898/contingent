package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.Addresses;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface AddressesRepository {

	List<Addresses> findAddresses(List<Long> nsiGlobalIds);
	List<Addresses> findActualAddresses(List<Long> nsiGlobalIds);
}
