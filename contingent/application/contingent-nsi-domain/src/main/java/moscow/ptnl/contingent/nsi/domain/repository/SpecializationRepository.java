package moscow.ptnl.contingent.nsi.domain.repository;

import moscow.ptnl.contingent.nsi.domain.area.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.nsi.domain.area.Specialization;
import org.springframework.data.repository.NoRepositoryBean;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@NoRepositoryBean
public interface SpecializationRepository {

    Specialization getByCode(String code);

}
