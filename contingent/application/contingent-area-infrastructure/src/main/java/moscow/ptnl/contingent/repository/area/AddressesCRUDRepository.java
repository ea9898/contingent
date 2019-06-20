package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.repository.CommonRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface AddressesCRUDRepository extends CommonRepository<Addresses, Long> {
}