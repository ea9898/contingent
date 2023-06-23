
package moscow.ptnl.contingent.nsi.domain.repository;

import moscow.ptnl.contingent.nsi.domain.area.MedicalOrganisationsOnko;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.NoRepositoryBean;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@NoRepositoryBean
public interface MedicalOrganisationsOnkoRepository {

    Optional<MedicalOrganisationsOnko> findByMoId(Long moId);
}
