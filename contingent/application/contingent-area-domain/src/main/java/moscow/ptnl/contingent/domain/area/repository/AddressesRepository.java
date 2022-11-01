package moscow.ptnl.contingent.domain.area.repository;

import moscow.ptnl.contingent.domain.area.entity.Addresses;
import org.springframework.data.domain.Page;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;

@NoRepositoryBean
public interface AddressesRepository {

	default List<Addresses> findAddresses(List<Long> nsiGlobalIds) {
		return findAddresses(nsiGlobalIds, null);
	}

	List<Addresses> findAddresses(List<Long> nsiGlobalIds, String aoLevel);

	List<Addresses> findAddresses(String areaOmkTeCodes, String regionTeCodes, String aoLevel);

	List<Addresses> findActualAddresses(List<Long> nsiGlobalIds);

	Set<Addresses> findActualAddresses(String streetCode, String planCode,
									   String placeCode, String cityCode, String areaCode, List<String> areaOmkTeCodes,
									   List<String> regionTeCodes, String aoLevel);

	default Addresses findAddressByGlobalId(Long globalId) {
		return findAddresses(Collections.singletonList(globalId)).stream().findFirst().orElse(null);
	}

	List<Addresses> saveAll(List<Addresses> addresses);

	Optional<Addresses> findAddressesByGlobalIdNsi(Long globalIdNsi);
}
