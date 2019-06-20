package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.entity.area.AreaAddress;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface AreaAddressRepository {

	List<AreaAddress> getActiveAreaAddresses(long moId, long areaTypeCode);

	List<AreaAddress> findAreaAddresses(List<Long> moAddressIds);

    List<AreaAddress> findAreaAddressesActual(List<Long> moAddressIds);

    List<AreaAddress> findAreaAddressesByAreaId(long areaId);

    List<AreaAddress> findAreaAddressByAddress(Addresses addresses);
}
