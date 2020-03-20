package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.entity.area.AreaAddress;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface AreaAddressRepository {

    List<AreaAddress> getActiveAreaAddresses(long moId, long areaTypeCode);

    List<AreaAddress> findAreaAddresses(List<Long> moAddressIds);

    List<AreaAddress> findAreaAddressesActual(List<Long> moAddressIds);

    Page<AreaAddress> findAreaAddressesByAreaId(Long moId, List<Long> areaIds, Pageable paging);

    List<AreaAddress> findAreaAddressByAddressIds(List<Long> addressIds);

}
