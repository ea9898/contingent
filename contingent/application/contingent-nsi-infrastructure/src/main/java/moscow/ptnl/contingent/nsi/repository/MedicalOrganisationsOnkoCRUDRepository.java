package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.nsi.domain.area.MedicalOrganisationsOnko;
import moscow.ptnl.contingent.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface MedicalOrganisationsOnkoCRUDRepository extends PagingAndSortingRepository<MedicalOrganisationsOnko, Long> {

    Optional<MedicalOrganisationsOnko> findByMoId(Long moId);
}
