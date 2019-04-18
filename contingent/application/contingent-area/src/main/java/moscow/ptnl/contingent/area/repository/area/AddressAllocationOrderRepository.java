package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.AddressAllocationOrder;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.Date;
import java.util.List;

@NoRepositoryBean
public interface AddressAllocationOrderRepository {

	List<AddressAllocationOrder> findAddressAllocationOrders(String number, Date date, String ouz, String name, Boolean archived);
}
