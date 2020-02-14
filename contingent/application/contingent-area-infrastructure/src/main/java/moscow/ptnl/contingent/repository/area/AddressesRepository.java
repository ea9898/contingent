package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.model.area.AddressLevelType;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.Collections;
import java.util.List;
import java.util.Set;

@NoRepositoryBean
public interface AddressesRepository {

	List<Addresses> findAddresses(List<Long> nsiGlobalIds);
	List<Addresses> findActualAddresses(List<Long> nsiGlobalIds);
	Set<Addresses> findActualAddresses(String streetCode, String planCode,
									   String placeCode, String cityCode, String areaCode, List<String> areaOmkTeCodes,
									   List<String> regionTeCodes, String aoLevel);

	default Addresses findAddressByGlobalId(Long globalId) {
		return findAddresses(Collections.singletonList(globalId)).stream().findFirst().orElse(null);
	}
}
