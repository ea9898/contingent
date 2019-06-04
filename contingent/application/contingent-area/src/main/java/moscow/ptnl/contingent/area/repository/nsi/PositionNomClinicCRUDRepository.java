package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.PositionNom;
import moscow.ptnl.contingent.repository.CommonRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface PositionNomClinicCRUDRepository extends CommonRepository<PositionNom, Long> {
}
