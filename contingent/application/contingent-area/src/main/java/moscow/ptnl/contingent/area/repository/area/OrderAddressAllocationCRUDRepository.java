package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.OrderAddressAllocation;
import moscow.ptnl.contingent.area.repository.CommonRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface OrderAddressAllocationCRUDRepository extends CommonRepository<OrderAddressAllocation, Long> {
}
