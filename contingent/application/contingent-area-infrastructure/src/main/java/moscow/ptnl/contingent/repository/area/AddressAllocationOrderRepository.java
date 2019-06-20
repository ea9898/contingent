package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.AddressAllocationOrders;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.NoRepositoryBean;

import java.time.LocalDate;
import java.util.List;

@NoRepositoryBean
public interface AddressAllocationOrderRepository {

	List<AddressAllocationOrders> findAddressAllocationOrders(String number, LocalDate date, String ouz, String name, Boolean archived);

	Page<AddressAllocationOrders> findAddressAllocationOrdersOverlapped(Long id, String number, LocalDate date, String name, Pageable paging);
}
