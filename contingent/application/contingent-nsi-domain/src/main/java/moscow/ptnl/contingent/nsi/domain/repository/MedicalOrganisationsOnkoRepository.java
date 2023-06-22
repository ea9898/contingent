
package moscow.ptnl.contingent.nsi.domain.repository;

import moscow.ptnl.contingent.nsi.domain.area.MedicalOrganisationsOnko;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface MedicalOrganisationsOnkoRepository extends CrudRepository<MedicalOrganisationsOnko, Long> {

    Optional<MedicalOrganisationsOnko> findByMoId(Long moId);
}
