package moscow.ptnl.contingent.domain.area.repository;

import moscow.ptnl.contingent.domain.area.entity.AddressAllocationOrders;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.NoRepositoryBean;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@NoRepositoryBean
public interface AddressAllocationOrderRepository {

	List<AddressAllocationOrders> findAddressAllocationOrders(String number, LocalDate date, String ouz, String name, Boolean archived);

	Page<AddressAllocationOrders> findAddressAllocationOrdersOverlapped(Long id, String number, LocalDate date, String name, Pageable paging);

	AddressAllocationOrders save(AddressAllocationOrders addressAllocationOrders);

	void delete(AddressAllocationOrders addressAllocationOrders);

	Optional<AddressAllocationOrders> findById(Long id);
}
